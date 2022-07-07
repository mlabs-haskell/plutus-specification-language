# Typing transaction families as HKDs

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module HKD where

import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)

import Families

data POSIXTime

~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell allows us, first of all, to move all
reusable declarations in a library module:

~~~ {.haskell.ignore}
module Families where

class ValidatorScript s where
  type Currency s :: k
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

class ValidatorScript s where
  type Currency s :: k
  type Datum s    :: Type
  type Redeemer s = (r :: Type) | r -> s
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
  type Currency ('Oracle n) = 'Token n
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell is

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
    wallet1 :: w ('Token m),
    wallet2 :: w ('Token n)}
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
