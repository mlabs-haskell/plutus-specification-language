## Specimens of transaction values

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, NumericUnderscores,
             PolyKinds, RankNTypes, TemplateHaskell, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Values where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)
import Refined (refineTH)

import Family
import Family.Values
import Family.Ledger (POSIXTime (POSIXTime), PubKey, Signature, always)
~~~
-->

That was the pain, now for the gain. We can supply a specific type parameter for `Inputs` and `Outputs` of the
transaction to fill in the details. In order to avoid name clashes, we're using a new module for the value-level
definitions and import the [type-level definitions](HKD):

~~~ {.haskell}
import HKD (
  ExchangeDApp(Oracle, CentralExchange),
  ExchangeInputs(ExchangeInputs, exchange, oracle1, oracle2, wallet1, wallet2),
  ExchangeOutputs(ExchangeOutputs, exchange, wallet1, wallet2),
  TransactionFamily(UpdateOracle, Exchange, DrainCollectedFees),
  Token(Token))
import qualified HKD as Type (OracleRedeemer (Update))
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
  DoUpdate :: OracleRedeemer n 'Type.Update
~~~

The remaining type definitions are project-generic and can be added to a library module:

~~~ {.haskell.ignore}
data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen "Collateral" '[ 'MinimumRequiredAda ],
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value '[ 'MinimumRequiredAda ],
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: dapp) -> Redeemer s -> Datum s -> ValueKnownBy dapp -> Type
data TxInputSpecimen s r d e where
  TxInputSpendingSpecimen :: TxOutSpecimen s d e -> RedeemerSpecimen s r -> TxInputSpecimen s ('Just r) d e
  TxInputReferenceSpecimen :: TxOutSpecimen s d e -> TxInputSpecimen s 'Nothing d e

type TxMintSpecimen :: forall (mp :: policy) -> MintRedeemer mp -> [MintOf mp] -> Type
data TxMintSpecimen mp r e = TxMintSpecimen {
  txMintValue :: MintValue e}

data WalletSpecimen name e = WalletSpecimen {
  walletPubKey :: PubKey,
  walletValue :: Value e}

type TxOutSpecimen :: forall (s :: dapp) -> Datum s -> ValueKnownBy dapp -> Type
data TxOutSpecimen s d e = TxOutSpecimen {
  txOutDatum :: DatumSpecimen s d,
  txOutValue :: Value e}

newtype MintValue qs = MintValue (AmountsOf qs)

newtype Value qs = Value (AmountsOf qs)

type SatisfiesQuantity :: Quantity c -> Type
data SatisfiesQuantity quantity deriving (Typeable)

type AmountOf :: q -> Type
data AmountOf quantity where
  Ada :: forall n. Refined (SatisfiesQuantity (RequiredAdaPlus n)) Natural -> AmountOf ('RequiredAdaPlus n)
  (:$) :: forall e (q :: Quantity e). Refined (SatisfiesQuantity q) Natural -> Proxy (UnitOf q) -> AmountOf q

type AmountsOf :: [q] -> Type
data AmountsOf quantities where
  (:+) :: AmountOf q -> AmountsOf qs -> AmountsOf (q ': qs)
  Destitute :: AmountsOf '[]
  MinimumAda :: AmountsOf '[ 'MinimumRequiredAda ]
  Whatever :: AmountsOf '[ 'AnythingElse ]
~~~

There is also a number of [`Predicate`](https://hackage.haskell.org/package/refined-0.7/docs/Refined.html#g:4) and
`UnitOf` instances for the `Quantity` and `MintQuantity` types. These allow compile-time (if statically known) and
run-time checks (otherwise) of the concrete amounts in values against the boundaries the [transaction
specification](HKD.md) requires there.

With all these types finally in place, we can generate concrete transactions as in the following example of an
`Exchange` transaction. The relative prices of assets specified by `Oracle 1` and `Oracle 2` are set in this example
to 45 and 60 Lovelace, respectively, so two users swap 60,000 of `Token 1` for 45,000 of `Token 2`. The transaction
diverts 1% of both amounts to the exchange.

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
    wallet1 = exampleWallet1Output,
    wallet2 = exampleWallet2Output},
  txMint = NoMints,
  txValidRange = always,
  txFee = Value MinimumAda,
  txSignatures = Map.fromList [(pubKey1, sig1), (pubKey2, sig2)]}

exampleExchangeInput :: TxInputSpecimen 'CentralExchange ('Just '()) '() '[ 'AnythingElse ]
exampleExchangeInput = TxInputSpendingSpecimen
  TxOutSpecimen {
    txOutDatum = Const (),
    txOutValue = Value Whatever}
  (Const ())
  
type ExchangeOutputAmounts = [ 'Some ('Token 1), 'Some ('Token 2), 'AnythingElse ]
exampleExchangeOutput :: TxOutSpecimen 'CentralExchange '() ExchangeOutputAmounts
exampleExchangeOutput = TxOutSpecimen {
  txOutDatum = Const (),
  txOutValue = Value ($$(refineTH 600) :$ Proxy @('Token 1) :+ $$(refineTH 450) :$ Proxy @('Token 2) :+ Whatever)
               :: Value ExchangeOutputAmounts}

exampleOracle1Input :: TxInputSpecimen ('Oracle 1) 'Nothing '() '[ 'Exactly 1 ('Token 1) ]
exampleOracle1Input = TxInputReferenceSpecimen
  TxOutSpecimen {
    txOutDatum = OracleDatum {
      priceInLovelace = 45,
      maxTradeVolume = 500_000,
      expiry = 20_000_000},
    txOutValue = Value ($$(refineTH 1) :$ Proxy @('Token 1) :+ Destitute :: AmountsOf '[ 'Exactly 1 ('Token 1) ])}

exampleOracle2Input :: TxInputSpecimen ('Oracle 2) 'Nothing '() '[ 'Exactly 1 ('Token 2) ]
exampleOracle2Input = TxInputReferenceSpecimen (
  TxOutSpecimen {
    txOutDatum = OracleDatum {
      priceInLovelace = 60,
      maxTradeVolume = 100_000,
      expiry = 20_000_000},
    txOutValue = Value ($$(refineTH 1) :$ Proxy @('Token 2) :+ Destitute) :: Value '[ 'Exactly 1 ('Token 2) ]})

exampleWallet1Input :: WalletSpecimen "Wallet 1" '[ 'Some ('Token 1) ]
exampleWallet1Input = WalletSpecimen pubKey1 $ Value ($$(refineTH 60_000) :$ Proxy @('Token 1) :+ Destitute)

exampleWallet2Input :: WalletSpecimen "Wallet 2" '[ 'Some ('Token 2) ]
exampleWallet2Input = WalletSpecimen pubKey2 (Value $ $$(refineTH 45_000) :$ Proxy @('Token 2) :+ Destitute)

exampleWallet1Output :: WalletSpecimen "Wallet 1" '[ 'Some ('Token 2) ]
exampleWallet1Output = WalletSpecimen pubKey1 (Value $ $$(refineTH 44_550) :$ Proxy @('Token 2) :+ Destitute)

exampleWallet2Output :: WalletSpecimen "Wallet 2" '[ 'Some ('Token 1) ]
exampleWallet2Output = WalletSpecimen pubKey2 (Value $ $$(refineTH 59_400) :$ Proxy @('Token 1) :+ Destitute)

pubKey1, pubKey2 :: PubKey
pubKey1 = "wallet1"
pubKey2 = "wallet2"

sig1, sig2 :: Signature
sig1 = "wallet1sig"
sig2 = "wallet2sig"

exampleCollateralWallet :: WalletSpecimen "Collateral" '[ 'MinimumRequiredAda ]
exampleCollateralWallet = WalletSpecimen pubKey2 (Value MinimumAda)
~~~

Once again, this example transaction is checked at compile time against [the specification](HKD.md). The example will
fail to compile if we forget an input or an output, or if we specify a wrong datum or redeemer, or even a wrong amount
of a specific token. This greatly improves the type-safety of the off-chain code. We can proceed to erase the types
and turn the `exampleExchangeTransaction` into a proper Cardano transaction and submit it.
