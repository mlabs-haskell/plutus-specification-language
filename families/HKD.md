# Typing transaction families as HKDs

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module HKD where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.Natural (Natural)

import Families

data POSIXTime

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
  type Inputs t  :: (forall s -> Redeemer s -> Type) -> (Economy t -> Type) -> Type
  type Mints t   :: (c -> Type) -> Type
  type Outputs t :: (DApp t -> Type) -> (Economy t -> Type) -> Type
  type Mints t = Const ()

type Wallet :: c -> k -> (c -> Type) -> Type
data Wallet c s w

type InputWallet :: c -> (forall s -> Redeemer s -> Type) -> (c -> Type) -> Type
data InputWallet c s w
~~~

The core project-specific datum and redeemer types don't change, nor do the `instance ValidatorScript` declarations.

<!--
~~~ {.haskell}
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Ada | Token Natural | Token :+ Token

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer (n :: Natural) = Trade | Update

instance ValidatorScript ('Oracle n) where
  type Currencies ('Oracle n) = '[ 'Token n ]
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript CentralExchange where
  type Currencies CentralExchange = '[ 'Ada ]
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell is wordier than it used to be, because
we need to declare two new ADTs for inputs and outputs of every script:

~~~ {.haskell}
data ExchangeDApp = Oracle Natural | CentralExchange

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy (t :: TransactionFamily) = Token

type UpdateOracleInputs :: Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> (c -> Type) -> Type
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('Oracle n) 'Update}
type UpdateOracleOutputs :: Natural -> (ExchangeDApp -> Type) -> (c -> Type) -> Type
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('Oracle n)}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

type ExchangeInputs :: Natural -> Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> (Token -> Type) -> Type
data ExchangeInputs m n s w = ExchangeInputs {
  exchange :: s 'CentralExchange '(),
  oracle1 :: s ('Oracle m) 'Trade,
  oracle2 :: s ('Oracle n) 'Trade,
  wallet1 :: w ('Token m :+ 'Ada),
  wallet2 :: w ('Token n :+ 'Ada)}
data ExchangeOutputs m n s w = ExchangeOutputs {
  exchange :: s 'CentralExchange,
  oracle1 :: s ('Oracle m),
  oracle2 :: s ('Oracle n),
  wallet1 :: w ('Token m :+ 'Ada),
  wallet2 :: w ('Token n :+ 'Ada)}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> (Token -> Type) -> Type
data DrainInputs s w = DrainInputs {
  echange :: s 'CentralExchange '()}
data DrainOutputs s w = DrainOutputs {
  echange :: s 'CentralExchange}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs
~~~

## Building concrete transactions

That was the pain, now for the gain. We can supply a specific type parameter for `Inputs` and `Outputs` of the
transaction to fill in the details. Furthermore, the following definitions are also project-generic: 

~~~ {.haskell.ignore}
data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen Ada,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value Ada,
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: script) -> Redeemer s -> Type
data TxInputSpecimen s r = TxInputSpecimen {
  txInputOut      :: TxOutSpecimen s,
  txInputRedeemer :: Redeemer s}

data TxMintSpecimen c = TxMintSpecimen {
  txMintValue :: Value c}

data WalletSpecimen c = WalletSpecimen {
  txInputWalletValue :: Value c}

data TxOutSpecimen s = TxOutSpecimen {
  txOutDatum :: Datum s,
  txOutValue :: Value (Currencies s)}

data Value currencies
~~~

We can use these definitions to generate concrete transactions

~~~ {.haskell}
exampleExchangeTransaction :: TxSpecimen ('Exchange 1 2)
exampleExchangeTransaction = TxSpecimen {
  txInputs = ExchangeInputs {
    exchange = exampleExchangeInput,
    oracle1 = exampleOracle1Input,
    oracle2 = exampleOracle2Input,
    wallet1 = exampleWallet1,
    wallet2 = exampleWallet2},
  txCollateral = exampleCollateralWallet,
  txOutputs = ExchangeOutputs {
    exchange = exampleExchangeOutput,
    oracle1 = exampleOracle1Output,
    oracle2 = exampleOracle2Output,
    wallet1 = exampleWallet1,
    wallet2 = exampleWallet2},
  txMint = Const (),
  txValidRange = undefined,
  txFee = exampleFee,
  txSignatures = Map.empty}

exampleExchangeInput :: TxInputSpecimen 'CentralExchange '()
exampleExchangeInput = TxInputSpecimen {
  txInputOut = exampleExchangeOutput,
  txInputRedeemer = ()}
  
exampleExchangeOutput = TxOutSpecimen {
  txOutDatum = (),
  txOutValue = undefined}

exampleOracle1Input :: TxInputSpecimen ('Oracle 1) 'Trade
exampleOracle1Input = undefined
exampleOracle2Input :: TxInputSpecimen ('Oracle 2) 'Trade
exampleOracle2Input = undefined

exampleOracle1Output :: TxOutSpecimen ('Oracle 1)
exampleOracle1Output = undefined
exampleOracle2Output :: TxOutSpecimen ('Oracle 2)
exampleOracle2Output = undefined

exampleWallet1 :: WalletSpecimen ('Token 1 :+ 'Ada)
exampleWallet1 = undefined
exampleWallet2 :: WalletSpecimen ('Token 2 :+ 'Ada)
exampleWallet2 = undefined
exampleCollateralWallet :: WalletSpecimen 'CollateralAda
exampleCollateralWallet = undefined

exampleFee :: Value Ada
exampleFee = undefined
~~~

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
