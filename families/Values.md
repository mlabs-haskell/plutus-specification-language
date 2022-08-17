## Specimens of transaction values

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, NumericUnderscores,
             PolyKinds, RankNTypes, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Values where

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

That was the pain, now for the gain. We can supply a specific type parameter for `Inputs` and `Outputs` of the
transaction to fill in the details. In order to avoid name clashes, we're using a new module for the value-level
definitions and import the [type-level definitions](HKD):

~~~ {.haskell}
import HKD (
  ExchangeDApp(Oracle, CentralExchange),
  ExchangeInputs(ExchangeInputs, exchange, oracle1, oracle2, wallet1, wallet2),
  ExchangeOutputs(ExchangeOutputs, exchange, oracle1, oracle2, wallet1, wallet2),
  TransactionFamily(UpdateOracle, Exchange, DrainCollectedFees),
  Token(Token))
import qualified HKD as Type (OracleRedeemer (Trade, Update))
~~~

Now we can declare the value-level types for different validator scripts:

~~~ {.haskell}
type instance RedeemerSpecimen 'CentralExchange = Const ()
type instance RedeemerSpecimen ('Oracle n) = OracleRedeemer n

type instance DatumSpecimen 'CentralExchange = Const ()
type instance DatumSpecimen ('Oracle n) = OracleDatum

data OracleDatum d where
  OracleDatum :: {
    priceInLovelace :: Natural,
    maxTradeVolume :: Natural,
    expiry :: POSIXTime
    }
    -> OracleDatum '()

data OracleRedeemer n r where
  DoTrade :: OracleRedeemer n 'Type.Trade
  DoUpdate :: OracleRedeemer n 'Type.Update
~~~

The remaining type definitions are project-generic and can be added to a library module:

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
  txInputRedeemer :: RedeemerSpecimen s r}

data TxMintSpecimen e = TxMintSpecimen {
  txMintValue :: Value e}

data WalletSpecimen e = WalletSpecimen {
  walletPubKey :: PubKey}

type TxOutSpecimen :: forall (s :: script) -> Datum s -> [currency] -> Type
data TxOutSpecimen s d e = TxOutSpecimen {
  txOutDatum :: DatumSpecimen s d,
  txOutValue :: Value e}

type Value :: [currency] -> Type
data Value currencies = Value (AmountsOf currencies)

type AmountsOf :: [c] -> Type
data AmountsOf currencies where
  Destitute :: AmountsOf '[]
  (:$) :: Natural -> Proxy c -> AmountsOf '[c]
  (:+) :: AmountsOf '[c] -> AmountsOf cs -> AmountsOf (c ': cs)
~~~

With all these types in place, we can generate concrete transactions as in the following example:

~~~ {.haskell}
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

exampleOracle1Input :: TxInputSpecimen ('Oracle 1) 'Type.Trade '() '[ 'Token 1 ]
exampleOracle1Input = TxInputSpecimen {
  txInputOut = TxOutSpecimen {
    txOutDatum = OracleDatum {
      priceInLovelace = 45,
      maxTradeVolume = 5_000,
      expiry = 20_000_000},
    txOutValue = Value (1 :$ Proxy @('Token 1))},
  txInputRedeemer = DoTrade}

exampleOracle2Input :: TxInputSpecimen ('Oracle 2) 'Type.Trade '() '[ 'Token 2 ]
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
