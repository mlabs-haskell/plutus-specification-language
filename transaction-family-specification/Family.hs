{-# LANGUAGE DataKinds, GADTs, PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Family where

import Data.Kind (Type)
import GHC.TypeLits (Natural, Symbol)

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

data MintOf mp = Mint Natural (MintedToken mp)
               | Burn Natural (MintedToken mp)
               | MintSome (MintedToken mp)
               | BurnSome (MintedToken mp)
               | MintOrBurnSome (MintedToken mp)

type InputFromScriptToTransaction t =
  forall (s :: DApp t) -> Maybe (Redeemer s) -> Datum s -> [Economy (DApp t)] -> Type
type OutputToScriptFromTransaction t =
  forall (s :: DApp t) -> Datum s -> [Economy (DApp t)] -> Type
type MintForTransaction t = forall (mp :: DApp t) -> MintRedeemer mp -> [MintOf mp] -> Type
type WalletUTxOFor dapp = Symbol -> [Economy dapp] -> Type

class Transaction (t :: familie) where
  type Inputs t  :: InputFromScriptToTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t   :: MintForTransaction t -> Type
  type Outputs t :: OutputToScriptFromTransaction t -> WalletUTxOFor (DApp t) -> Type
  type Mints t = NoMints (DApp t)

type NoMints :: forall k -> (forall (mp :: k) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
data NoMints t mp = NoMints

-- type/kind synonyms to simplify the kind signatures in specifications 
type InputsFor dapp = (forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> [Economy dapp] -> Type) -> (Symbol -> [Economy dapp] -> Type) -> Type
type MintsFor dapp = (forall (mp :: dapp) -> MintRedeemer mp -> [MintOf mp] -> Type) -> Type
type OutputsFor dapp = (forall (s :: dapp) -> Datum s -> [Economy dapp] -> Type) -> (Symbol -> [Economy dapp] -> Type) -> Type

data Ada = Ada
