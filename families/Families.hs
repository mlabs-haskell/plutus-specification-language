{-# LANGUAGE DataKinds, ExplicitForAll, PolyKinds, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}

module Families where

import Data.Kind (Type)

class ValidatorScript (s :: script) where
  type Currency s :: k
  type Datum s    :: Type
  type Redeemer s :: Type

class Transaction (t :: familie) where
  type Inputs t  :: [Type]
  type Mints t   :: [k]
  type Outputs t :: [Type]
  type Mints t = '[]

{-
type Input :: forall (script :: k) -> Redeemer script -> Type
data Input script redeemer = Input
data Output (script :: k)

data WalletInput currency
data WalletOutput currency

data c1 :+ c2
-}
