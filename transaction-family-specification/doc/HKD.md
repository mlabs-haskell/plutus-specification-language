# Typing transaction families as HKDs

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures, NoStarIsType,
             PolyKinds, RankNTypes, TypeApplications, TypeFamilies, UndecidableInstances #-}

module HKD where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import GHC.TypeNats (Natural)

import Family
~~~
-->

The new HKD representation of the transaction family and DApp types in Haskell allows us, first of all, to move all
reusable declarations into a library module:

~~~ {.haskell.ignore}
module Family where

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

type MintOf mp = MintQuantity (MintedToken mp)

data MintQuantity currency
  = Mint Natural currency
  | Burn Natural currency
  | MintSome currency
  | BurnSome currency
  | MintOrBurnSome currency

type ValueKnownBy dapp = [Quantity (Economy dapp)]

data Quantity currency
  = Exactly Natural currency
  | AtLeast Natural currency
  | AtMost Natural currency
  | Some currency
  | RequiredAdaPlus Natural
  | MinimumRequiredAda
  | AnythingElse
  deriving (Show, Typeable)

type InputFromScriptToTransaction t =
  forall (s :: DApp t) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy (DApp t) -> Type
type OutputToScriptFromTransaction t =
  forall (s :: DApp t) -> Datum s -> ValueKnownBy (DApp t) -> Type
type MintForTransaction t = forall (mp :: DApp t) -> MintRedeemer mp -> [MintOf mp] -> Type
type WalletUTxOFor dapp = Symbol -> ValueKnownBy dapp -> Type

class Transaction (t :: familie) where
  type Inputs t  :: InputFromScriptToTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t   :: MintForTransaction t -> Type
  type Outputs t :: OutputToScriptFromTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
data NoMints t mp = NoMints

-- type/kind synonyms to simplify the kind signatures in specifications 
type InputsFor dapp = (forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy dapp -> Type) -> (Symbol -> ValueKnownBy dapp -> Type) -> Type
type MintsFor dapp = (forall (mp :: dapp) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
type OutputsFor dapp = (forall (s :: dapp) -> Datum s -> ValueKnownBy dapp -> Type) -> (Symbol -> ValueKnownBy dapp -> Type) -> Type
~~~

The core `data TransactionFamily` and `instance ValidatorScript` declarations remain unchanged.

<!--
~~~ {.haskell}
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Token Natural deriving (Eq, Typeable)

data OracleRedeemer (n :: Natural) = Update

instance ValidatorScript ('Oracle n) where
  type Currencies ('Oracle n) = '[ 'Token n ]
  type Datum ('Oracle n) = ()
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript 'CentralExchange where
  type Currencies 'CentralExchange = '[ 'AnythingElse ]
  type Datum 'CentralExchange = ()
  type Redeemer 'CentralExchange = ()
~~~
-->

## Running example

The new HKD type representation is wordier than it [used to be](Typed.md), because we need to declare two new ADTs for
inputs and outputs of every script, and each of them requires a kind signature:

~~~ {.haskell}
data ExchangeDApp = Oracle Natural | CentralExchange

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy ExchangeDApp = Token

type UpdateOracleInputs :: Natural -> InputsFor ExchangeDApp
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('Oracle n) ('Just 'Update) '() '[ 'Exactly 1 ('Token n), 'MinimumRequiredAda ]}
type UpdateOracleOutputs :: Natural -> OutputsFor ExchangeDApp
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('Oracle n) '() '[ 'Exactly 1 ('Token n), 'MinimumRequiredAda ]}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

type ExchangeInputs :: Natural -> Natural -> InputsFor ExchangeDApp
data ExchangeInputs m n s w = ExchangeInputs {
  exchange :: s 'CentralExchange ('Just '()) '() '[ 'AnythingElse ],
  oracle1 :: s ('Oracle m) 'Nothing '() '[ 'Exactly 1 ('Token m) ],
  oracle2 :: s ('Oracle n) 'Nothing '() '[ 'Exactly 1 ('Token n) ],
  wallet1 :: w "Alice" '[ 'Some ('Token m) ],
  wallet2 :: w "Bob" ' [ 'Some ('Token n) ]}
type ExchangeOutputs :: Natural -> Natural -> OutputsFor ExchangeDApp
data ExchangeOutputs m n s w = ExchangeOutputs {
  exchange :: s 'CentralExchange '() ['Some ('Token m), 'Some ('Token n), 'AnythingElse],
  wallet1 :: w "Alice" '[ 'Some ('Token n) ],
  wallet2 :: w "Bob" '[ 'Some ('Token m) ]}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: InputsFor ExchangeDApp
data DrainInputs s w = DrainInputs {
  exchange :: s 'CentralExchange ('Just '()) '() '[ 'AnythingElse ]}
type DrainOutputs :: OutputsFor ExchangeDApp
data DrainOutputs s w = DrainOutputs {
  authority :: w "Owner" '[ 'AnythingElse ],
  exchange :: s 'CentralExchange '() '[ 'MinimumRequiredAda ]}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs
~~~

Replacing type families and their instances by data families would be slightly less verbose, but they'd bring other
problems in turn.

If that's not clear, the code snippet above exists solely at the type level of Haskell. One reason for that is because
Haskell is not a dependently-typed language at the value level, but at the type level it is. Every field of the
`Inputs`/`Outputs` records above has a kind of `InputFromScriptToTransaction`.`OutputToScriptFromTransaction`, which
rely on [visible dependent
quantification](https://ryanglscott.github.io/2019/03/15/visible-dependent-quantification-in-haskell), a GHC extension
available only in kinds.

## Graphing the transaction diagram

Once the transaction types are declared, we can automatically [generate diagrams](Diagram.md) for them.

## Building concrete transaction values

The other reason for declaring the transactions at the [type
level](https://aphyr.com/posts/342-typing-the-technical-interview) is because types _can_ be used to constrain
values. So after we declare the transaction types, we can build [transaction values](Values.md) conforming to these
types.

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
