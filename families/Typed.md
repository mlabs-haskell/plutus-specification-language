# Typing transaction families

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module Typed where

import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)

import Families

data POSIXTime

~~~
-->

The [preceding concepts](TransactionFamily.md) were relying on transactions,
UTxOs, and scripts without defining them. They could be built on top of the
actual live transactions on the blockchain. But that's not what we're after
here, we're interested in abstracted transactions.

Here are some properties we can safely abstract away:

* Actual numerical or string values of script hashes and addresses. We're only
  interested in their identities, which can be represented with identifiers.
* The quantities inside values, for the most part: we do care what token types
  were involved, how many of each type rarely matters. More precisely, we may
  want a static assurance that there is:
  - no given token,
  - exactly one token,
  - zero or one token, or
  - an arbitrary positive amount of tokens.
* The values of UTxO datums and redeemers; we're only interested in their types.
* Transaction signatories, certificates, datum hashes, and similar proofs can
  just be assumed to be present.
* Transaction validity range.

## Example

Imagine a DApp that consists of N+1 scripts: N being oracles that maintain
the going price of different tokens and one an exchange that gets its prices
from the oracles.

* Each oracle can be updated by its authority, providing for N different
  transaction types that involve only that oracle.
* An exchange transaction involves the exchange script, two oracles to look
  up the prices, and one or two wallets.
* A percentage of every exchange gets collected as a fee, which can be
  drained by the exchange authority in a separate transaction.

Once abstracted of specific amounts and other details, all different
transactions listed above make up a transaction family of N²+N+1 transaction
types. We can represent these types in Haskell as follows.

~~~ {.haskell}
data MyDApp = Oracle Natural | CentralExchange
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Ada | Token Natural | Token :+ Token
type instance DApp ('UpdateOracle n) = MyDApp
type instance DApp ('Exchange m n) = MyDApp
type instance DApp 'DrainCollectedFees = MyDApp
type instance Economy (t :: TransactionFamily) = Token

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer (n :: Natural) = Trade | Update

instance ValidatorScript ('Oracle n) where
  type Currency ('Oracle n) = 'Token n
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()

type UpdateOracleInputs :: Natural -> (forall (s :: MyDApp) -> Redeemer s -> Type) -> (c -> Type) -> Type
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('Oracle n) 'Update}
type UpdateOracleOutputs :: Natural -> (MyDApp -> Type) -> (c -> Type) -> Type
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('Oracle n)}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

-- type ExchangeInputs :: Natural -> Natural -> (MyDApp -> Redeemer s -> Type) -> (c -> Type) -> Type
type ExchangeInputs :: Natural -> Natural -> (forall (s :: MyDApp) -> Redeemer s -> Type) -> (Token -> Type) -> Type
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
    wallet1 :: w ('Token m),
    wallet2 :: w ('Token n)}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: (forall (s :: MyDApp) -> Redeemer s -> Type) -> (Token -> Type) -> Type
data DrainInputs s w = DrainInputs {
  echange :: s 'CentralExchange '()}
data DrainOutputs s w = DrainOutputs {
  echange :: s 'CentralExchange}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs
~~~

The above declarations are already something that could be automatically
converted into a diagram. They also have a side benefit of being valid Haskell
declarations that can be verified and built upon.

The instance declarations depend on the following definitions, presented here
with kind signatures restricted to the example transaction family but
otherwise reusable across different transaction families and DApps:

~~~ {.haskell.ignore}
class ValidatorScript (script :: DApp) where
  type Currency script :: Token
  type Datum script :: Type
  type Redeemer script :: Type

class Transaction (t :: TransactionFamily) where
  type Inputs t :: [Type]
  type Mints t :: [Type]
  type Outputs t :: [Type]
  type Mints t = '[]
~~~

Note the dependent kind quantification here, necessary because the redeemer
type depends on the script:

~~~ {.haskell.ignore}
type Input :: forall (script :: DApp) -> Redeemer script -> Type
data Input script redeemer = Input
data Output (script :: DApp)

data WalletInput currency
data WalletOutput currency

data c1 :+ c2
~~~

## Building concrete transactions

A `Transaction` instance such as `'Exchange` describes only a general shape of
the transaction. We can fill in the details using

~~~ {.haskell}
data TxInstance t where
  TxInstance :: Transaction t => {
    txInputs :: Inputs t TxInputInstance WalletInstance,
    txCollateral :: WalletInstance Ada,
    txOutputs :: Outputs t TxOutInstance WalletInstance,
    txMint :: Mints t TxMintInstance,
    txValidRange :: !SlotRange,
    txFee :: Value Ada,
    txSignatures :: Map PubKey Signature}
    -> TxInstance t

type TxInputInstance :: forall (s :: script) -> Redeemer s -> Type
data TxInputInstance s r where
  TxInputInstance :: ValidatorScript s => {
    txInputOut      :: TxOutInstance s,
    txInputRedeemer :: Redeemer s}
    -> TxInputInstance s r

data TxMintInstance c where
  TxMintInstance :: {
    txMintValue :: Value c}
    -> TxMintInstance c

data WalletInstance c where
  WalletInstance :: {
    txInputWalletValue :: Value c}
    -> WalletInstance s

data TxOutInstance s where
  TxOutInstance :: ValidatorScript s => {
    txOutDatum :: Datum s,
    txOutValue :: Value (Currency s)}
    -> TxOutInstance s

data Value currencies
~~~

<!--
~~~ {.haskell}
data PubKey
data Signature
data SlotRange
~~~
-->

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
