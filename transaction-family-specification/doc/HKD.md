# Typing transaction families as HKDs

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures, NoStarIsType,
             PolyKinds, RankNTypes, TypeApplications, TypeFamilies, UndecidableInstances #-}

module HKD where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Numeric.Natural (Natural)

import Family
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell allows us, first of all, to move all
reusable declarations into a library module:

~~~ {.haskell.ignore}
module Families where

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s    :: Type
  type Redeemer s :: Type

class MintingPolicyScript s where
  type MintedToken s  :: Type
  type MintRedeemer s :: Type

type DApp :: fam -> Type
type family DApp t

type Economy :: dapp -> Type
type family Economy t

type InputFromScriptToTransaction t =
  forall (s :: DApp t) -> Maybe (Redeemer s) -> Datum s -> [Economy (DApp t)] -> Type
type OutputToScriptFromTransaction t =
  forall (s :: DApp t) -> Datum s -> [Economy (DApp t)] -> Type
type MintForTransaction t = forall (mp :: DApp t) -> MintRedeemer mp -> [MintedToken mp] -> Type
type WalletUTxOFor dapp = Symbol -> [Economy dapp] -> Type

class Transaction (t :: familie) where
  type Inputs t  :: InputFromScriptToTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t   :: MintForTransaction t -> Type
  type Outputs t :: OutputToScriptFromTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintedToken mp] -> Type) -> Type
data NoMints t mp = NoMints

-- type/kind synonyms to simplify the kind signatures in specifications 
type InputsFor dapp = (forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> [Economy dapp] -> Type) -> (Symbol -> [Economy dapp] -> Type) -> Type
type MintsFor dapp = (forall (mp :: dapp) -> MintRedeemer mp -> [MintedToken mp] -> Type) -> Type
type OutputsFor dapp = (forall (s :: dapp) -> Datum s -> [Economy dapp] -> Type) -> (Symbol -> [Economy dapp] -> Type) -> Type
~~~

The core `data TransactionFamily` and `instance ValidatorScript` declarations remain unchanged.
`Trade` redeemer any longer because

<!--
~~~ {.haskell}
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Token Natural | ScriptAda

data OracleRedeemer (n :: Natural) = Update

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
type instance Economy ExchangeDApp = Token

type UpdateOracleInputs :: Natural -> InputsFor ExchangeDApp
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('Oracle n) ('Just 'Update) '() '[ 'Exactly 1 ('Token n) ]}
type UpdateOracleOutputs :: Natural -> OutputsFor ExchangeDApp
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('Oracle n) '() '[ 'Exactly 1 ('Token n) ]}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

type ExchangeInputs :: Natural -> Natural -> InputsFor ExchangeDApp
data ExchangeInputs m n s w = ExchangeInputs {
  exchange :: s 'CentralExchange ('Just '()) '() '[],
  oracle1 :: s ('Oracle m) 'Nothing '() '[ 'Exactly 1 ('Token m) ],
  oracle2 :: s ('Oracle n) 'Nothing '() '[ 'Exactly 1 ('Token n) ],
  wallet1 :: w "Wallet 1" '[ 'Some ('Token m) ],
  wallet2 :: w "Wallet 2" ' [ 'Some ('Token n) ]}
type ExchangeOutputs :: Natural -> Natural -> OutputsFor ExchangeDApp
data ExchangeOutputs m n s w = ExchangeOutputs {
  exchange :: s 'CentralExchange '() ['Some ('Token m), 'Some ('Token n)],
  wallet1 :: w "Wallet 1" '[ 'Some ('Token n) ],
  wallet2 :: w "Wallet 2" '[ 'Some ('Token m) ]}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: InputsFor ExchangeDApp
data DrainInputs s w = DrainInputs {
  exchange :: s 'CentralExchange ('Just '()) '() ['Some ('Token 1), 'Some ('Token 2)]}
type DrainOutputs :: OutputsFor ExchangeDApp
data DrainOutputs s w = DrainOutputs {
  authority :: w "Owner's wallet" ['Some ('Token 1), 'Some ('Token 2)],
  exchange :: s 'CentralExchange '() '[]}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs
~~~

## Graphing the transaction diagram

[Diagram](Diagram.md)

## Building concrete transaction values

[Getting concrete](Values.md)

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
