{-# LANGUAGE DataKinds, ExplicitForAll, GADTs,
             PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances #-}

module Family where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)

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

data Ada = Ada
