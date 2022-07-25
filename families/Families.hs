{-# LANGUAGE DataKinds, ExplicitForAll, GADTs,
             PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances #-}

module Families where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Data.Proxy (Proxy)
import Ledger (PubKey, Signature, SlotRange)

class ValidatorScript s where
  type Currencies s :: [k]
  type Datum s    :: Type
  type Redeemer s = (r :: Type) | r -> s

type DApp :: fam -> Type
type family DApp t

type Economy :: fam -> Type
type family Economy t

class Transaction (t :: familie) where
  type Inputs t  :: (forall (s :: DApp t) -> Redeemer s -> [Economy t] -> Type) -> ([Economy t] -> Type) -> Type
  type Mints t   :: ([Economy t] -> Type) -> Type
  type Outputs t :: (DApp t -> [Economy t] -> Type) -> ([Economy t] -> Type) -> Type
  type Mints t = Const ()

data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen Collateral,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value '[ 'Ada ],
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: script) -> Redeemer s -> [currency] -> Type
data TxInputSpecimen s r e = TxInputSpecimen {
  txInputOut      :: TxOutSpecimen s e,
  txInputRedeemer :: Redeemer s}

data TxMintSpecimen e = TxMintSpecimen {
  txMintValue :: Value e}

data WalletSpecimen e = WalletSpecimen {
  walletPubKey :: PubKey}

data TxOutSpecimen s e = TxOutSpecimen {
  txOutDatum :: Datum s,
  txOutValue :: Value e}

type Value :: [currency] -> Type
data Value currencies = Value (AmountsOf currencies)

type AmountsOf :: [c] -> Type
data AmountsOf currencies where
  Destitute :: AmountsOf '[]
  (:$) :: Natural -> Proxy c -> AmountsOf '[c]
  (:+) :: AmountsOf '[c] -> AmountsOf cs -> AmountsOf (c ': cs)

infixr 3 :$
infixr 2 :+

data Ada = Ada

data Collateral = CollateralAda

type Wallet :: c -> k -> (c -> Type) -> Type
data Wallet c s w

type InputWallet :: c -> (forall s -> Redeemer s -> c -> Type) -> (c -> Type) -> Type
data InputWallet c s w
