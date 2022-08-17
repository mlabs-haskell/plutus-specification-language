{-# LANGUAGE DataKinds, ExplicitForAll, GADTs,
             PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances #-}

module FamilyValues where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Data.Proxy (Proxy)
import Ledger (PubKey, Signature, SlotRange)
import Families (Ada(Ada), Datum, Redeemer, Transaction(Inputs, Mints, Outputs))

type DatumSpecimen :: forall script -> Datum script -> Type
type family DatumSpecimen s :: Datum s -> Type

type RedeemerSpecimen :: forall script -> Redeemer script -> Type
type family RedeemerSpecimen s :: Redeemer s -> Type

data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen Collateral,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value '[ 'Ada ],
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: script) -> Redeemer s -> Datum s -> [currency] -> Type
data TxInputSpecimen s r d e = TxInputSpecimen {
  txInputOut      :: TxOutSpecimen s d e,
  txInputRedeemer :: RedeemerSpecimen s r}

data TxMintSpecimen e = TxMintSpecimen {
  txMintValue :: Value e}

data WalletSpecimen e = WalletSpecimen {
  walletPubKey :: PubKey}

type TxOutSpecimen :: forall (s :: script) -> Datum s -> [currency] -> Type
data TxOutSpecimen s d e = TxOutSpecimen {
  txOutDatum :: DatumSpecimen s d,
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

data Collateral = CollateralAda
