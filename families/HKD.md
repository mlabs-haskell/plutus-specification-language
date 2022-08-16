# Typing transaction families as HKDs

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, NumericUnderscores,
             PolyKinds, RankNTypes, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module HKD where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)

import Families
import Ledger (POSIXTime (POSIXTime), PubKey, always)
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell allows us, first of all, to move all
reusable declarations into a library module:

~~~ {.haskell.ignore}
module Families where

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s    :: Type
  type Redeemer s = (r :: Type) | r -> s

type DApp :: fam -> Type
type family DApp t

type Economy :: fam -> Type
type family Economy t

class Transaction (t :: familie) where
  type Inputs t  :: (forall s -> Redeemer s -> Datum s -> Type) -> ([Economy t] -> Type) -> Type
  type Mints t   :: ([Economy t] -> Type) -> Type
  type Outputs t :: (forall s -> Datum s -> Type) -> ([Economy t] -> Type) -> Type
  type Mints t = Const ()

type Wallet :: c -> k -> (c -> Type) -> Type
data Wallet c s w

type InputWallet :: c -> (forall s -> Redeemer s -> Datum s -> Type) -> (c -> Type) -> Type
data InputWallet c s w
~~~

The core `data TransactionFamily` and `instance ValidatorScript` declarations remain unchanged.

<!--
~~~ {.haskell}
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Token Natural | ScriptAda

data OracleRedeemer (n :: Natural) = Trade | Update

instance ValidatorScript ('Oracle n) where
  type Currencies ('Oracle n) = '[ 'Token n ]
  type Datum ('Oracle n) = ()
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript 'CentralExchange where
  type Currencies 'CentralExchange = '[ 'Ada ]
  type Datum 'CentralExchange = ()
  type Redeemer 'CentralExchange = ()
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell is wordier than it used to be, because
we need to declare two new ADTs for inputs and outputs of every script:

~~~ {.haskell}
data ExchangeDApp = Oracle Natural | CentralExchange

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy (t :: TransactionFamily) = Token

type UpdateOracleInputs :: Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Datum s -> [Token] -> Type) -> (c -> Type) -> Type
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('Oracle n) 'Update '() '[ 'Token n ]}
type UpdateOracleOutputs :: Natural -> (forall (s :: ExchangeDApp) -> Datum s -> [Token] -> Type) -> (c -> Type) -> Type
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('Oracle n) '() '[ 'Token n ]}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

type ExchangeInputs :: Natural -> Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Datum s -> [Token] -> Type) -> ([Token] -> Type) -> Type
data ExchangeInputs m n s w = ExchangeInputs {
  exchange :: s 'CentralExchange '() '() '[],
  oracle1 :: s ('Oracle m) 'Trade '() '[ 'Token m ],
  oracle2 :: s ('Oracle n) 'Trade '() '[ 'Token n ],
  wallet1 :: w '[ 'Token m ],
  wallet2 :: w ' ['Token n ]}
type ExchangeOutputs :: Natural -> Natural -> (forall (s :: ExchangeDApp) -> Datum s -> [Token] -> Type) -> ([Token] -> Type) -> Type
data ExchangeOutputs m n s w = ExchangeOutputs {
  exchange :: s 'CentralExchange '() ['Token m, 'Token n],
  oracle1 :: s ('Oracle m) '() '[ 'Token m ],
  oracle2 :: s ('Oracle n) '() '[ 'Token n ],
  wallet1 :: w '[ 'Token n ],
  wallet2 :: w '[ 'Token m ]}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: (forall (s :: ExchangeDApp) -> Redeemer s -> Datum s -> [Token] -> Type) -> ([Token] -> Type) -> Type
data DrainInputs s w = DrainInputs {
  exchange :: s 'CentralExchange '() '() ['Token 1, 'Token 2]}
type DrainOutputs :: (forall (s :: ExchangeDApp) -> Datum s -> [Token] -> Type) -> ([Token] -> Type) -> Type
data DrainOutputs s w = DrainOutputs {
  authority :: w ['Token 1, 'Token 2],
  exchange :: s 'CentralExchange '() '[]}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs

minAda :: AmountsOf '[ 'ScriptAda ]
minAda = 2_000_000 :$ Proxy @'ScriptAda
~~~

## Graphing the transaction diagram

[Diagram](Diagram.md)

## Building concrete transactions

That was the pain, now for the gain. We can supply a specific type parameter for `Inputs` and `Outputs` of the
transaction to fill in the details. Furthermore, the following definitions are also project-generic and can be added
to a library module:

~~~ {.haskell.ignore}
data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen Collateral,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value '[ 'Ada ],
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: script) -> Redeemer s -> Datum s -> [currency] -> Type
data TxInputSpecimen s r d e = TxInputSpecimen {
  txInputOut      :: TxOutSpecimen s d e,
  txInputRedeemer :: Redeemer s}

data TxMintSpecimen c = TxMintSpecimen {
  txMintValue :: Value c}

data WalletSpecimen e = WalletSpecimen {
  walletPubKey :: PubKey}

type TxOutSpecimen :: forall (s :: script) -> Datum s -> [currency] -> Type
data TxOutSpecimen s d e = TxOutSpecimen {
  txOutDatum :: DatumSpecimen s d,
  txOutValue :: Value e}

type Value :: [currency] -> Type
data Value currencies = Value (NatMap currencies)

type NatMap :: [t] -> Type
type family NatMap xs where
  NatMap '[] = ()
  NatMap (x ': xs) = (Natural, NatMap xs)
~~~

These type definitions can be used to generate concrete transactions as in the following example:

~~~ {.haskell}
type instance RedeemerSpecimen 'CentralExchange = Const ()
type instance RedeemerSpecimen ('Oracle n) = OracleRedeemerSpecimen n

type instance DatumSpecimen 'CentralExchange = Const ()
type instance DatumSpecimen ('Oracle n) = OracleDatum

data OracleDatum d where
  OracleDatum :: {
    priceInLovelace :: Natural,
    maxTradeVolume :: Natural,
    expiry :: POSIXTime
    }
    -> OracleDatum '()

data OracleRedeemerSpecimen n r where
  DoTrade :: OracleRedeemerSpecimen n 'Trade
  DoUpdate :: OracleRedeemerSpecimen n 'Update

exampleExchangeTransaction :: TxSpecimen ('Exchange 1 2)
exampleExchangeTransaction = TxSpecimen {
  txInputs = ExchangeInputs {
    exchange = exampleExchangeInput,
    oracle1 = exampleOracle1Input,
    oracle2 = exampleOracle2Input,
    wallet1 = exampleWallet1Input,
    wallet2 = exampleWallet2Input},
  txCollateral = exampleCollateralWallet,
  txOutputs = ExchangeOutputs {
    exchange = exampleExchangeOutput,
    oracle1 = exampleOracle1Output,
    oracle2 = exampleOracle2Output,
    wallet1 = exampleWallet1Output,
    wallet2 = exampleWallet2Output},
  txMint = Const (),
  txValidRange = always,
  txFee = exampleFee,
  txSignatures = Map.empty}

exampleExchangeInput :: TxInputSpecimen 'CentralExchange '() '() '[]
exampleExchangeInput = TxInputSpecimen {
  txInputOut = TxOutSpecimen {
    txOutDatum = Const (),
    txOutValue = Value Destitute},
  txInputRedeemer = Const ()}
  
exampleExchangeOutput :: TxOutSpecimen 'CentralExchange '() ['Token 1, 'Token 2]
exampleExchangeOutput = TxOutSpecimen {
  txOutDatum = Const (),
  txOutValue = Value (1 :$ Proxy @('Token 1) :+ 1 :$ Proxy @('Token 2))}

exampleOracle1Input :: TxInputSpecimen ('Oracle 1) 'Trade '() '[ 'Token 1 ]
exampleOracle1Input = TxInputSpecimen {
  txInputOut = TxOutSpecimen {
    txOutDatum = OracleDatum {
      priceInLovelace = 45,
      maxTradeVolume = 5_000,
      expiry = 20_000_000},
    txOutValue = Value (1 :$ Proxy @('Token 1))},
  txInputRedeemer = DoTrade}

exampleOracle2Input :: TxInputSpecimen ('Oracle 2) 'Trade '() '[ 'Token 2 ]
exampleOracle2Input = TxInputSpecimen {
  txInputOut = TxOutSpecimen {
    txOutDatum = OracleDatum {
      priceInLovelace = 60,
      maxTradeVolume = 10_000,
      expiry = 20_000_000},
    txOutValue = Value (1 :$ Proxy @('Token 2))},
  txInputRedeemer = DoTrade}

exampleOracle1Output :: TxOutSpecimen ('Oracle 1) '() '[ 'Token 1 ]
exampleOracle1Output = TxOutSpecimen {
  txOutDatum = OracleDatum {
    priceInLovelace = 45,
    maxTradeVolume = 3_000,
    expiry = 20_000_000},
  txOutValue = Value (1 :$ Proxy @('Token 1))}

exampleOracle2Output :: TxOutSpecimen ('Oracle 2) '() '[ 'Token 2 ]
exampleOracle2Output = TxOutSpecimen {
  txOutDatum = OracleDatum {
    priceInLovelace = 60,
    maxTradeVolume = 8_500,
    expiry = 20_000_000},
  txOutValue = Value (1 :$ Proxy @('Token 2))}

exampleWallet1Input :: WalletSpecimen '[ 'Token 1 ]
exampleWallet1Input = WalletSpecimen pubKey1

exampleWallet1Output :: WalletSpecimen '[ 'Token 2 ]
exampleWallet1Output = WalletSpecimen pubKey1

exampleWallet2Input :: WalletSpecimen '[ 'Token 2 ]
exampleWallet2Input = WalletSpecimen pubKey2

exampleWallet2Output :: WalletSpecimen '[ 'Token 1 ]
exampleWallet2Output = WalletSpecimen pubKey2

pubKey1, pubKey2 :: PubKey
pubKey1 = "wallet1"
pubKey2 = "wallet2"

exampleCollateralWallet :: WalletSpecimen Collateral
exampleCollateralWallet = undefined

exampleFee :: Value '[ 'Ada ]
exampleFee = undefined
~~~

We can proceed to erase the types and turn the `exampleExchangeTransaction` into a proper Cardano transaction and
submit it. This would improve the type-safety of the off-chain code.

## Closing the family

Is this transaction family open or closed? A most likely design would have the
oracles open to other exchanges and other kinds of dApps, so at least the
`Trade` redeemer would be accessible to anyone. However there is no value in any
`Value` locked by an oracle, so this opening is not much of a vulnerability
itself. We just need to ensure that every oracle verifies that its
`priceInLovelace` cannot be changed by any transaction using this redeemer.

The `CentralExchange` script on the other hand would likely be parameterized
by a whitelist of accepted oracles. Unfortunately the same method can't be
applied in the opposite direction: if we tried to *also* supply the
`CentralExchange` script address as a parameter to every oracle there'd be a
cyclic dependency. We can't close the type family this way.

One workaround is to use an NFT whose sole token is carried by our
`CentralExchange`. The NFT identity can be supplied as a parameter to every
oracle as well as the exchange, so there's no cyclic dependency. With this
addition we can close our dApp so it looks [as follows](NFT.md).
