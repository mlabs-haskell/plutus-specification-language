{-# LANGUAGE DataKinds, ExplicitForAll, ExistentialQuantification, PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators #-}

module Families where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)

class ValidatorScript s where
  type Currencies s :: [k]
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

data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen 'CollateralAda,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value Ada,
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: script) -> Redeemer s -> Type
data TxInputSpecimen s r = TxInputSpecimen {
  txInputOut      :: TxOutSpecimen s,
  txInputRedeemer :: Redeemer s}

data TxMintSpecimen c = TxMintSpecimen {
  txMintValue :: Value c}

data WalletSpecimen c = WalletSpecimen {
  txInputWalletValue :: Value c}

data TxOutSpecimen s = TxOutSpecimen {
  txOutDatum :: Datum s,
  txOutValue :: Value (Currencies s)}

data Value currencies

data Ada

data Collateral = CollateralAda

type Wallet :: c -> k -> (c -> Type) -> Type
data Wallet c s w

type InputWallet :: c -> (forall s -> Redeemer s -> Type) -> (c -> Type) -> Type
data InputWallet c s w

data PubKey
data Signature
data SlotRange
