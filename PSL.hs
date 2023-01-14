{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module PSL where

import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import MonoidDo qualified
import Plutarch.Core
import Plutarch.PType
import Plutarch.Prelude

data PBool (ef :: PTypeF) = PTrue | PFalse
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

x :: PConstructable edsl PBool => Term edsl PBool
x = pcon PTrue

{- | Newtype used to conveniently give a datatype the 'PReprPrimitive'
 representation. Not intended to be used directly!

 @
 data PSth (ef :: PTypeF) ... deriving (PHasRepr) via PIsPrimitive
 @
-}
data PIsPrimitive (ef :: PTypeF)

instance PHasRepr PIsPrimitive where
  type PReprSort _ = PReprPrimitive

data PInteger (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PValue (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PUTXO (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PUTXORef (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PTokenName (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PCurrencySymbol (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PTimeRange (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PPubKeyHash (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PAddress (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PByteString (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

data PData (ef :: PTypeF)
  = PDataConstr (Pf ef PInteger) (Pf ef (PList PData))
  | PDataMap (Pf ef (PList (PPair PData PData)))
  | PDataList (Pf ef (PList PData))
  | PDataInteger (Pf ef PInteger)
  | PDataByteString (Pf ef PByteString)
  deriving (PHasRepr) via PIsPrimitive

data POwnUTXO d (ef :: PTypeF) = POwnUTXO
  { value :: Pf ef PValue
  , datum :: Pf ef d
  }
  deriving (PHasRepr) via PIsPrimitive

data PDiagram (datum :: PType) (ef :: PTypeF) deriving (PHasRepr) via PIsPrimitive

class (forall a. PConstructable edsl a => PConstructable edsl (f a)) => PConstructable2 edsl f

instance (forall a. PConstructable edsl a => PConstructable edsl (f a)) => PConstructable2 edsl f

data PList a ef
  = PNil
  | PCons (ef /$ a) (ef /$ PList a)
  deriving (PHasRepr) via PIsPrimitive

data PNat f = PZ | PS (Pf f PNat)
  deriving (PHasRepr) via PIsPrimitive

instance PConstructable edsl PNat => Num (Term edsl PNat) where
  fromInteger 0 = pcon PZ
  fromInteger n
    | n > 0 = pcon $ PS (fromInteger (n - 1))
    | otherwise = error "Can't construct a negative natural number"
  _ + _ = error "Can't add natural numbers"
  _ - _ = error "Can't subtract natural numbers"
  _ * _ = error "Can't multiply natural numbers"
  negate _ = error "Can't negate a natural number"
  abs = id
  signum = const 1

class
  ( forall d. Monoid (Term edsl (PDiagram d))
  , (forall d. IsPType edsl d => IsPType edsl (PDiagram d))
  , Monoid (Term edsl PValue)
  , PDSL edsl
  , forall a b. (IsPType edsl a, IsPType edsl b) => PConstructable' edsl (a #-> b)
  , forall a b. (IsPType edsl a, IsPType edsl b) => PConstructable' edsl (PPair a b)
  , forall a b. (IsPType edsl a, IsPType edsl b) => PConstructable' edsl (PEither a b)
  , forall a. (PIsSOP edsl a) => PConstructable' edsl (PSOPed a)
  , PConstructable' edsl PUnit
  , Num (Term edsl PInteger)
  , IsString (Term edsl PByteString)
  , IsPType edsl PInteger
  , IsPType edsl PByteString
  , IsPType edsl PValue
  , IsPType edsl PUTXO
  , IsPType edsl PUTXORef
  , IsPType edsl PTokenName
  , IsPType edsl PCurrencySymbol
  , IsPType edsl PTimeRange
  , IsPType edsl PPubKeyHash
  , IsPType edsl PAddress
  , PConstructable edsl PNat
  , PConstructable edsl PData
  , forall d. IsPType edsl d => PConstructable edsl (POwnUTXO d)
  , forall a. IsPType edsl a => PConstructable edsl (PList a)
  ) =>
  PPSL edsl
  where
  -- input: WalletInput (pkh, value), ProtocolInput (sh, datum, value), OwnInput (datum, redeemer, value)
  -- output: WalletOutput (pkh, value), ProtocolOutput (sh, datum, value), OwnOutput (datum, value)
  -- mint: MintOwn (token, int), Mint (symbol, token, int)
  -- other: DCert (cert), ValidIn (range), SignedBy (pkh)
  requireInput :: Term edsl PUTXORef -> Term edsl (PDiagram d)
  requireOwnInput :: Term edsl (POwnUTXO d) -> Term edsl (PDiagram d)
  createOwnOutput :: Term edsl (POwnUTXO d) -> Term edsl (PDiagram d)
  witnessOutput :: Term edsl PUTXO -> Term edsl (PDiagram d)
  createOutput :: Term edsl PUTXO -> Term edsl (PDiagram d)
  mintOwn :: Term edsl PTokenName -> Term edsl PInteger -> Term edsl (PDiagram d)
  witnessMint :: Term edsl PCurrencySymbol -> Term edsl PTokenName -> Term edsl PInteger -> Term edsl (PDiagram d)
  requireSignature :: Term edsl PPubKeyHash -> Term edsl (PDiagram d)
  requireValidRange :: Term edsl PTimeRange -> Term edsl (PDiagram d)
  toProtocol :: Protocol p d => Proxy p -> Term edsl d -> Term edsl PValue -> Term edsl PUTXO
  toAddress :: Term edsl PAddress -> Term edsl PValue -> Term edsl PData -> Term edsl PUTXO
  fromPkh :: Term edsl PPubKeyHash -> Term edsl PAddress
  utxoRefIs :: Term edsl PUTXORef -> Term edsl PUTXO -> Term edsl (PDiagram d)
  emptyValue :: Term edsl PValue
  mkValue :: Term edsl PCurrencySymbol -> Term edsl PTokenName -> Term edsl PInteger -> Term edsl PValue
  mkAda :: Term edsl PInteger -> Term edsl PValue
  mkOwnValue :: Term edsl PTokenName -> Term edsl PInteger -> Term edsl PValue

data Specification d where
  Specification ::
    forall d (caseType :: PType).
    ( forall edsl.
      PPSL edsl =>
      Term edsl caseType ->
      Term edsl (PDiagram d)
    ) ->
    Specification d

class Protocol p d | p -> d where
  protocolName :: Proxy p -> String
  specification :: Proxy p -> Specification d

data CounterDatum f = CounterDatum
  { counter :: Pf f PNat
  , addr :: Pf f PAddress
  , datum :: Pf f PData
  }
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

data CounterCase f where
  CounterStep :: Pf f CounterDatum -> Pf f PValue -> CounterCase f
  CounterConsume :: Pf f PAddress -> Pf f PData -> Pf f PValue -> CounterCase f
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

counterCases :: PPSL edsl => Term edsl CounterCase -> Term edsl (PDiagram CounterDatum)
counterCases c = pmatch c \case
  CounterStep datum' value -> MonoidDo.do
    createOwnOutput $ pcon $ POwnUTXO value datum'
  CounterConsume addr outdatum value -> MonoidDo.do
    requireOwnInput $ pcon $ POwnUTXO value (pcon $ CounterDatum {counter = pcon PZ, addr, datum = outdatum})
    createOutput $ toAddress addr value outdatum

data CounterProtocol

instance Protocol CounterProtocol CounterDatum where
  protocolName _ = "Counter"
  specification _ = Specification @CounterDatum @CounterCase counterCases

data ExampleDatum (ef :: PTypeF) = ExampleDatum (Pf ef PPubKeyHash)
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

data ExampleCase (ef :: PTypeF) where
  ExampleConsume ::
    Pf ef PNat ->
    Pf ef PValue ->
    Pf ef PValue ->
    Pf ef PPubKeyHash ->
    Pf ef PUTXORef ->
    ExampleCase ef
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

exampleCases :: PPSL edsl => Term edsl ExampleCase -> Term edsl (PDiagram ExampleDatum)
exampleCases c = pmatch c \case
  ExampleConsume counter value' value pkh otherinput -> MonoidDo.do
    requireOwnInput $ pcon $ POwnUTXO value (pcon $ ExampleDatum pkh)
    requireInput $ otherinput
    utxoRefIs otherinput $ toProtocol (Proxy @CounterProtocol) (pcon $ CounterDatum counter (fromPkh pkh) (pcon $ PDataList $ pcon $ PNil)) value'
    witnessOutput $ toProtocol (Proxy @CounterProtocol) (pcon $ CounterDatum 5 (fromPkh pkh) (pcon $ PDataList $ pcon $ PNil)) (value' <> value)

-- observeOutput $ undefined (canonical $ Proxy @CounterProtocol) value' (pcon $ CounterDatum 5 pkh)

data MaksDatum f = MaksA | MaksB
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

data MaksCase (ef :: PTypeF) where
  -- given one A, we produce an A and a B
  MaksFork :: {ada :: Pf ef PInteger, ada' :: Pf ef PInteger} -> MaksCase ef
  -- given one A and one B, we lock the value of both into the counter protocol
  MaksConsume :: {ada :: Pf ef PInteger, ada' :: Pf ef PInteger} -> MaksCase ef
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

pkh :: Term edsl PPubKeyHash
pkh = undefined

maksCases :: forall edsl. PPSL edsl => Term edsl MaksCase -> Term edsl (PDiagram MaksDatum)
maksCases c = pmatch c \case
  MaksFork {ada, ada'} -> MonoidDo.do
    requireOwnInput $ pcon $ POwnUTXO (mkAda ada) (pcon MaksA)
    createOwnOutput $ pcon $ POwnUTXO (mkAda ada) (pcon MaksA)
    createOwnOutput $ pcon $ POwnUTXO (mkAda ada') (pcon MaksB)
  MaksConsume {ada, ada'} -> MonoidDo.do
    requireOwnInput $ pcon $ POwnUTXO (mkAda ada) (pcon MaksA)
    requireOwnInput $ pcon $ POwnUTXO (mkAda ada') (pcon MaksB)
    createOutput $ toProtocol (Proxy @CounterProtocol) (pcon $ CounterDatum 100 (fromPkh pkh) (pcon $ PDataList $ pcon $ PNil)) (mkAda ada <> mkAda ada')

data MaksProtocol

instance Protocol MaksProtocol MaksDatum where
  protocolName _ = "Mak's Protocol"
  specification _ = Specification @MaksDatum @MaksCase maksCases
