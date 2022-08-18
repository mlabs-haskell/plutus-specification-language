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

type DApp :: fam -> Type
type family DApp t

type Economy :: fam -> Type
type family Economy t

class Transaction (t :: familie) where
  type Inputs t  :: (forall (s :: DApp t) -> Redeemer s -> Datum s -> [Economy t] -> Type)
                 -> ([Economy t] -> Type)
                 -> Type
  type Mints t   :: ([Economy t] -> Type) -> Type
  type Outputs t :: (forall (s :: DApp t) -> Datum s -> [Economy t] -> Type)
                 -> ([Economy t] -> Type)
                 -> Type
  type Mints t = Const ()

type Wallet :: c -> k -> (c -> Type) -> Type
data Wallet c s w

type InputWallet :: c -> (forall s -> Redeemer s -> Datum s -> c -> Type) -> (c -> Type) -> Type
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
