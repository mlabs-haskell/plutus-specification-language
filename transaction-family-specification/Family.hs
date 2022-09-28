{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Family where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Natural, Symbol)

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s :: Type
  type Redeemer s :: Type
  type Currencies s = '[]

class MintingPolicyScript s where
  type MintedToken s :: Type
  type MintRedeemer s :: Type

type DApp :: fam -> Type
type family DApp t

type Economy :: dApp -> Type
type family Economy t

type MintOf mp = MintQuantity Natural (MintedToken mp)

data MintQuantity count currency
  = Mint count currency
  | Burn count currency
  | MintSome currency
  | BurnSome currency
  | MintOrBurnSome currency

type ValueKnownBy dApp = [Quantity (Economy dApp)]

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

type WalletUTxOFor dApp = Symbol -> ValueKnownBy dApp -> Type

class Transaction (t :: familie) where
  type Inputs t :: InputFromScriptToTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t :: MintForTransaction t -> Type
  type Outputs t :: OutputToScriptFromTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
data NoMints t mp = NoMints

-- type/kind synonyms to simplify the kind signatures in specifications
type InputsFor dApp = (forall (s :: dApp) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy dApp -> Type) -> (Symbol -> ValueKnownBy dApp -> Type) -> Type

type MintsFor dApp = (forall (mp :: dApp) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type

type OutputsFor dApp = (forall (s :: dApp) -> Datum s -> ValueKnownBy dApp -> Type) -> (Symbol -> ValueKnownBy dApp -> Type) -> Type
