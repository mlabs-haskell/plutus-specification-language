{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Family.Values where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import Family
  ( Datum,
    Economy,
    MintOf,
    MintQuantity (Burn, BurnSome, Mint, MintOrBurnSome, MintSome),
    MintRedeemer,
    Quantity (AnythingElse, AtLeast, AtMost, Exactly, MinimumRequiredAda, RequiredAdaPlus, Some),
    Redeemer,
    Transaction (Inputs, Mints, Outputs),
    ValueKnownBy,
  )
import Family.Ledger (POSIXTime, PubKey, Signature, SlotRange, always)
import GHC.TypeLits (ErrorMessage (Text), Symbol, TypeError)
import GHC.TypeNats (KnownNat, Natural, natVal)
import Refined

type DatumSpecimen :: forall script -> Datum script -> Type
type family DatumSpecimen s :: Datum s -> Type

type RedeemerSpecimen :: forall script -> Redeemer script -> Type
type family RedeemerSpecimen s :: Redeemer s -> Type

data TxSpecimen t = TxSpecimen
  { txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
    txCollateral :: WalletSpecimen "Collateral" '[ 'MinimumRequiredAda],
    txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
    txMint :: Mints t TxMintSpecimen,
    txValidRange :: !SlotRange,
    txFee :: Value '[ 'MinimumRequiredAda],
    txSignatures :: Map PubKey Signature
  }

type TxInputSpecimen :: forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy dapp -> Type
data TxInputSpecimen s r d e where
  TxInputSpendingSpecimen :: TxOutSpecimen s d e -> RedeemerSpecimen s r -> TxInputSpecimen s ('Just r) d e
  TxInputReferenceSpecimen :: TxOutSpecimen s d e -> TxInputSpecimen s 'Nothing d e

type TxMintSpecimen :: forall (mp :: policy) -> MintRedeemer mp -> [MintOf mp] -> Type
data TxMintSpecimen mp r e = TxMintSpecimen
  { txMintValue :: MintValue e
  }

data WalletSpecimen name e = WalletSpecimen
  { walletPubKey :: PubKey,
    walletValue :: Value e
  }

type TxOutSpecimen :: forall (s :: dapp) -> Datum s -> ValueKnownBy dapp -> Type
data TxOutSpecimen s d e = TxOutSpecimen
  { txOutDatum :: DatumSpecimen s d,
    txOutValue :: Value e
  }

newtype MintValue qs = MintValue (AmountsOf qs)

type SatisfiesMintQuantity :: MintQuantity n c -> Type
data SatisfiesMintQuantity quantity deriving (Typeable)

instance
  forall n c q.
  (KnownNat n, Typeable c, Typeable q) =>
  Predicate (SatisfiesMintQuantity (Mint n (q :: c) :: MintQuantity Natural c)) Integer
  where
  validate q m
    | m == fromIntegral n = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed mint amount in value, wrong amount"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall n c q.
  (KnownNat n, Typeable c, Typeable q) =>
  Predicate (SatisfiesMintQuantity (Burn n (q :: c) :: MintQuantity Natural c)) Integer
  where
  validate q m
    | -m == fromIntegral n = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed burn amount in value, wrong amount"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall c q.
  (Typeable c, Typeable q) =>
  Predicate (SatisfiesMintQuantity (MintSome (q :: c) :: MintQuantity Natural c)) Integer
  where
  validate q m
    | m > 0 = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed mint amount in value, non-positive amount"

instance
  forall c q.
  (Typeable c, Typeable q) =>
  Predicate (SatisfiesMintQuantity (BurnSome (q :: c) :: MintQuantity Natural c)) Integer
  where
  validate q m
    | m < 0 = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed burn amount in value, non-negative amount"

instance
  forall c q.
  (Typeable c, Typeable q) =>
  Predicate (SatisfiesMintQuantity (MintOrBurnSome (q :: c) :: MintQuantity Natural c)) Integer
  where
  validate q m
    | m /= 0 = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed mint/burn amount in value, zero amount"

newtype Value qs = Value (AmountsOf qs)

type SatisfiesQuantity :: Quantity c -> Type
data SatisfiesQuantity quantity deriving (Typeable)

instance
  forall n c q.
  (KnownNat n, Typeable c) =>
  Predicate (SatisfiesQuantity (RequiredAdaPlus n :: Quantity c)) (AmountOf q)
  where
  validate p (Ada m)
    | unrefine m == n = success
    | otherwise = throwRefineOtherException (typeRep p) "Disallowed amount in value, wrong Ada amount"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall n c q.
  (KnownNat n, Typeable c, Typeable q) =>
  Predicate (SatisfiesQuantity (Exactly n (q :: c) :: Quantity c)) Natural
  where
  validate q m
    | m == n = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed amount in value, wrong amount"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall n c q.
  (KnownNat n, Typeable c, Typeable q) =>
  Predicate (SatisfiesQuantity (AtLeast n q :: Quantity c)) Natural
  where
  validate q m
    | m >= n = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed amount in value, amount too small"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall n c q.
  (KnownNat n, Typeable c, Typeable q) =>
  Predicate (SatisfiesQuantity (AtMost n q :: Quantity c)) Natural
  where
  validate q m
    | m <= n = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed amount in value, amount too large"
    where
      n = natVal @n (undefined :: Proxy n)

instance
  forall c q.
  (Typeable c, Typeable q) =>
  Predicate (SatisfiesQuantity (Some q :: Quantity c)) Natural
  where
  validate q m
    | m > 0 = success
    | otherwise = throwRefineOtherException (typeRep q) "Disallowed amount in value, zero"

type UnitOf :: Quantity c -> c
type family UnitOf q where
  UnitOf ('Exactly n coin) = coin
  UnitOf ('AtLeast n coin) = coin
  UnitOf ('AtMost n coin) = coin
  UnitOf ('Some coin) = coin
  UnitOf _ = TypeError (Text "(:$) works only with a concrete non-Ada coin")

type AmountOf :: q -> Type
data AmountOf quantity where
  Ada :: forall n. Refined (SatisfiesQuantity (RequiredAdaPlus n)) Natural -> AmountOf ('RequiredAdaPlus n)
  (:$) :: forall e (q :: Quantity e). Refined (SatisfiesQuantity q) Natural -> Proxy (UnitOf q) -> AmountOf q

type AmountsOf :: [q] -> Type
data AmountsOf quantities where
  (:+) :: AmountOf q -> AmountsOf qs -> AmountsOf (q ': qs)
  Destitute :: AmountsOf '[]
  MinimumAda :: AmountsOf '[ 'MinimumRequiredAda]
  Whatever :: AmountsOf '[ 'AnythingElse]

infixr 2 :+

infixr 3 :$
