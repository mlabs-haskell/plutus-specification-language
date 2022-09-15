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
import GHC.TypeLits (Natural, Symbol)

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s :: Type
  type Redeemer s :: Type

class MintingPolicyScript s where
  type MintedToken s :: Type
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
  | RequiredAda Natural
  | MinimumRequiredAda
  | AnythingElse

type InputFromScriptToTransaction t =
  forall (s :: DApp t) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy (DApp t) -> Type

type OutputToScriptFromTransaction t =
  forall (s :: DApp t) -> Datum s -> ValueKnownBy (DApp t) -> Type

type MintForTransaction t = forall (mp :: DApp t) -> MintRedeemer mp -> [MintOf mp] -> Type

type WalletUTxOFor dapp = Symbol -> ValueKnownBy dapp -> Type

class Transaction (t :: familie) where
  type Inputs t :: InputFromScriptToTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t :: MintForTransaction t -> Type
  type Outputs t :: OutputToScriptFromTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
data NoMints t mp = NoMints

-- type/kind synonyms to simplify the kind signatures in specifications
type InputsFor dapp = (forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy dapp -> Type) -> (Symbol -> ValueKnownBy dapp -> Type) -> Type

type MintsFor dapp = (forall (mp :: dapp) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type

type OutputsFor dapp = (forall (s :: dapp) -> Datum s -> ValueKnownBy dapp -> Type) -> (Symbol -> ValueKnownBy dapp -> Type) -> Type
