{-# LANGUAGE DataKinds, ExplicitForAll, PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators #-}

module Families where

import Data.Functor.Const (Const)
import Data.Kind (Type)

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
