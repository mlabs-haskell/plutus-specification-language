{-# LANGUAGE DataKinds, ExplicitForAll, GADTs,
             PolyKinds, RankNTypes, StandaloneKindSignatures,
             TypeFamilies, TypeFamilyDependencies, TypeOperators,
             UndecidableInstances #-}

module Family.Values where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Numeric.Natural (Natural)
import GHC.TypeLits (Symbol)
import Family (Ada(Ada), Datum, Economy, Redeemer, MintRedeemer, MintedToken, Transaction(Inputs, Mints, Outputs))
import Family.Ledger (PubKey, Signature, SlotRange, POSIXTime, always)

type DatumSpecimen :: forall script -> Datum script -> Type
type family DatumSpecimen s :: Datum s -> Type

type RedeemerSpecimen :: forall script -> Redeemer script -> Type
type family RedeemerSpecimen s :: Redeemer s -> Type

data TxSpecimen t = TxSpecimen {
  txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
  txCollateral :: WalletSpecimen "Collateral" Collateral,
  txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
  txMint :: Mints t TxMintSpecimen,
  txValidRange :: !SlotRange,
  txFee :: Value '[ 'Ada ],
  txSignatures :: Map PubKey Signature}

type TxInputSpecimen :: forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> [Economy dapp] -> Type
data TxInputSpecimen s r d e where
  TxInputSpendingSpecimen :: TxOutSpecimen s d e -> RedeemerSpecimen s r -> TxInputSpecimen s ('Just r) d e
  TxInputReferenceSpecimen :: TxOutSpecimen s d e -> TxInputSpecimen s 'Nothing d e

type TxMintSpecimen :: forall (mp :: policy) -> MintRedeemer mp -> [MintedToken mp] -> Type
data TxMintSpecimen mp r e = TxMintSpecimen {
  txMintValue :: Value e}

data WalletSpecimen name e = WalletSpecimen {
  walletPubKey :: PubKey}

type TxOutSpecimen :: forall (s :: dapp) -> Datum s -> [Economy dapp] -> Type
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
