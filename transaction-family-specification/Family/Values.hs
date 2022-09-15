{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Family.Values where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy)
import Family (Datum, Economy, MintOf, MintRedeemer, Redeemer, Transaction (Inputs, Mints, Outputs),
               ValueKnownBy, Quantity (AnythingElse, MinimumRequiredAda))
import Family.Ledger (POSIXTime, PubKey, Signature, SlotRange, always)
import GHC.TypeLits (Symbol)
import Numeric.Natural (Natural)

type DatumSpecimen :: forall script -> Datum script -> Type
type family DatumSpecimen s :: Datum s -> Type

type RedeemerSpecimen :: forall script -> Redeemer script -> Type
type family RedeemerSpecimen s :: Redeemer s -> Type

data TxSpecimen t = TxSpecimen
  { txInputs :: Inputs t TxInputSpecimen WalletSpecimen,
    txCollateral :: WalletSpecimen "Collateral" '[ 'MinimumRequiredAda ],
    txOutputs :: Outputs t TxOutSpecimen WalletSpecimen,
    txMint :: Mints t TxMintSpecimen,
    txValidRange :: !SlotRange,
    txFee :: Value '[ 'MinimumRequiredAda ],
    txSignatures :: Map PubKey Signature
  }

type TxInputSpecimen :: forall (s :: dapp) -> Maybe (Redeemer s) -> Datum s -> ValueKnownBy dapp -> Type
data TxInputSpecimen s r d e where
  TxInputSpendingSpecimen :: TxOutSpecimen s d e -> RedeemerSpecimen s r -> TxInputSpecimen s ('Just r) d e
  TxInputReferenceSpecimen :: TxOutSpecimen s d e -> TxInputSpecimen s 'Nothing d e

type TxMintSpecimen :: forall (mp :: policy) -> MintRedeemer mp -> [MintOf mp] -> Type
data TxMintSpecimen mp r e = TxMintSpecimen
  { txMintValue :: Value e
  }

data WalletSpecimen name e = WalletSpecimen
  { walletPubKey :: PubKey
  }

type TxOutSpecimen :: forall (s :: dapp) -> Datum s -> ValueKnownBy dapp -> Type
data TxOutSpecimen s d e = TxOutSpecimen
  { txOutDatum :: DatumSpecimen s d,
    txOutValue :: Value e
  }

type Value :: [currency] -> Type
data Value currencies = Value (AmountsOf currencies)

type AmountsOf :: [c] -> Type
data AmountsOf currencies where
  Destitute :: AmountsOf '[]
  MinimumAda :: AmountsOf '[ 'MinimumRequiredAda ]
  Whatever :: AmountsOf '[ 'AnythingElse ]
  (:$) :: Natural -> Proxy c -> AmountsOf '[c]
  (:+) :: AmountsOf '[c] -> AmountsOf cs -> AmountsOf (c ': cs)

infixr 3 :$

infixr 2 :+
