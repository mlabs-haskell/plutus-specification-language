{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Family where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s    :: Type
  type Redeemer s :: Type

class MintingPolicyScript s where
  type MintedToken s  :: Type
  type MintRedeemer s :: Type

type DApp :: fam -> Type
type family DApp t

type Economy :: fam -> Type
type family Economy t

class Transaction (t :: familie) where
  type Inputs t  :: (forall (s :: DApp t) -> Maybe (Redeemer s) -> Datum s -> [Economy t] -> Type)
                 -> (Symbol -> [Economy t] -> Type)
                 -> Type
  type Mints t   :: (forall (mp :: DApp t) -> MintRedeemer mp -> [MintedToken mp] -> Type) -> Type
  type Outputs t :: (forall (s :: DApp t) -> Datum s -> [Economy t] -> Type)
                 -> (Symbol -> [Economy t] -> Type)
                 -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintedToken mp] -> Type) -> Type
data NoMints t mp = NoMints

data Ada = Ada
