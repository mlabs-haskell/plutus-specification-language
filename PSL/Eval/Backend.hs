{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Eval.Backend where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
  asks,
  local,
 )
import Control.Monad.Trans.State.Strict (State, evalState, get, state)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX (
  POSIXTime,
  posixSecondsToUTCTime,
 )
import Data.Time.Format.ISO8601 qualified as Time
import Data.Type.Equality (type (:~:))
import Generics.SOP (
  Injection,
  NP,
  SOP,
  injections,
 )
import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP
import Generics.SOP.NP (
  cata_NP,
  collapse_NP,
  ctraverse'_NP,
  map_NP,
  pure_NP,
  zipWith_NP,
 )
import Generics.SOP.NS (
  collapse_SOP,
  index_NS,
  index_SOP,
  map_SOP,
  traverse'_SOP,
 )
import PSL
import PSL.Eval.Interval (Interval)
import PSL.Eval.Interval qualified as Iv
import PSL.Eval.TH (
  mkConvHelper,
  mkConvHelper',
  unsound,
 )
import Plutarch.Core
import Plutarch.Frontends.Data
import Plutarch.PType
import Plutarch.Repr.SOP
import Prettyprinter (Doc, Pretty (pretty), (<+>))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P
import Type.Reflection (type (:~:) (Refl))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Backend types ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The context for evaluation.
type EvalM = ReaderT (Map Text Int) (State Int)

runEvalM :: EvalM a -> a
runEvalM = flip evalState 0 . flip runReaderT Map.empty

takeEvalM :: EvalM (EvalM a -> a)
takeEvalM = do
  st <- lift get
  env <- ask
  pure (flip evalState st . flip runReaderT env)

-- | A computation that returns an evaluated value.
newtype Eval ty = Eval (EvalM Val)

-- | The NbE backend.
type EK = 'PDSLKind Eval

instance PDSL EK where
  newtype IsPTypePrimData EK _ = PTy Ty
  newtype PEffect EK a = PEff (Identity a)
    deriving (Functor, Applicative, Monad) via Identity

unPure :: PEffect EK a -> a
unPure (PEff (Identity x)) = x

type TypeReprInfo :: forall (a :: PType). PHs a -> Constraint
class TypeReprInfo ty where
  typeReprInfo :: Proxy ty -> Ty

instance TypeReprInfo ty => IsPTypePrim EK ty where
  isPTypePrim = PTy (typeReprInfo (Proxy @ty))

class (IsPType EK a) => TypeInfo a
instance (IsPType EK a) => TypeInfo a

typeInfo :: forall a. TypeInfo a => Proxy a -> Ty
typeInfo _ = case isPType @EK @_ @a of
  IsPTypeData (PTy ty) -> ty

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall a. EvalM Val -> Term EK a
pattern MkTerm {runTerm} <- (Term (Eval runTerm))

term :: EvalM Val -> Term EK a
term x = Term (Eval x)

pterm :: Val -> Term EK a
pterm t = term $ pure t

data Ident
  = Ident Int Text -- case
  | IdentU Int -- eta
  | IdentM Text -- meta
  deriving stock (Eq, Ord)

newtype Uid = Uid Int
  deriving stock (Eq, Ord)

withIdent :: Text -> (Ident -> EvalM a) -> EvalM a
withIdent nm f = do
  n <- asks (fromMaybe 0 . Map.lookup nm)
  local (Map.alter (Just . maybe 1 (+ 1)) nm) (f $ Ident n nm)

freshUid :: EvalM Uid
freshUid = lift $ state (\n -> (Uid n, n + 1))

uidToIdent :: Uid -> Ident
uidToIdent (Uid n) = IdentU n

data Decision = Decision Neutral [(Pat, Val)]
  deriving stock (Eq, Ord)

data Spine = Spine Neutral [Val]
  deriving stock (Eq, Ord)

data Projection = Projection Neutral Text
  deriving stock (Eq, Ord)

data SOPChoice xs = SOPChoice (NP (SOP.K Ident) xs) Val

data SOPDec = forall a. PGeneric a => SOPDec (Proxy a) Neutral (NP SOPChoice (PCode a))

instance Eq SOPDec where
  SOPDec (pa :: Proxy a) nx x == SOPDec (pb :: Proxy b) ny y = case sopEqT pa pb of
    Left _ -> False
    Right Refl -> nx == ny && choiceCompare x y == EQ

instance Ord SOPDec where
  compare (SOPDec (pa :: Proxy a) nx x) (SOPDec (pb :: Proxy b) ny y) = case sopEqT pa pb of
    Left ord -> ord
    Right Refl -> case compare nx ny of
      EQ -> choiceCompare x y
      x -> x

choiceCompare :: NP SOPChoice xs -> NP SOPChoice xs -> Ordering
choiceCompare SOP.Nil SOP.Nil = EQ
choiceCompare (x SOP.:* xs) (y SOP.:* ys) = case comp x y of
  EQ -> choiceCompare xs ys
  x -> x
  where
    comp (SOPChoice xs vx) (SOPChoice ys vy) = case npCompare xs ys of
      EQ -> compare vx vy
      x -> x

data Neutral
  = NDec Decision
  | NDecSOP SOPDec
  | NProj Projection
  | NSp Spine
  | NId Ident
  | NPrim PrimOp
  deriving stock (Eq, Ord)

data PrimOp
  = PrimAdd Val Val
  | PrimSub Val Val
  | PrimMul Val Val
  | PrimNeg Neutral
  | PrimSignum Neutral
  | PrimAbs Neutral
  | PrimConcat Val Val
  | PrimIvIntersection Val Val
  deriving stock (Eq, Ord)

data Partial a
  = PNormal a
  | PNeutral Neutral
  deriving stock (Eq, Ord, Functor)

binPrim :: (a -> a -> b) -> (Val -> Val -> PrimOp) -> (a -> Val) -> (Partial a -> Partial a -> Partial b)
binPrim norm _ _ (PNormal a) (PNormal b) = PNormal (a `norm` b)
binPrim _ neu to a b = pprim $ toVal to a `neu` toVal to b

uniPrim :: (a -> b) -> (Neutral -> PrimOp) -> (Partial a -> Partial b)
uniPrim norm _ (PNormal a) = PNormal (norm a)
uniPrim _ neu (PNeutral a) = pprim (neu a)

instance Num (Partial Integer) where
  (+) = binPrim (+) PrimAdd VInt
  (-) = binPrim (-) PrimSub VInt
  (*) = binPrim (*) PrimMul VInt
  negate = uniPrim negate PrimNeg
  abs = uniPrim abs PrimAbs
  signum = uniPrim signum PrimSignum
  fromInteger = PNormal

instance Semigroup (Partial BS) where
  (<>) = binPrim (<>) PrimConcat VBS

instance Semigroup (Partial Value) where
  (<>) = binPrim (<>) PrimConcat VValue

instance Semigroup (Partial Diagram) where
  (<>) = binPrim (<>) PrimConcat VDiagram

data Natural = NZ | NS (Partial Natural)
  deriving stock (Eq, Ord)

data List a = LNil | LCons a (Partial (List a))
  deriving stock (Eq, Ord, Functor)

data Lam = Lam Uid (Val -> Val)

instance Eq Lam where
  Lam m _ == Lam n _ = m == n

instance Ord Lam where
  Lam m _ <= Lam n _ = m <= n

data SOPed v = forall a. PGeneric a => SOPed (Proxy a) (SOP (SOP.K v) (PCode a))

instance Ord v => Eq (SOPed v) where
  SOPed (pa :: Proxy a) x == SOPed (pb :: Proxy b) y = case sopEqT pa pb of
    Left _ -> False
    Right Refl -> sopCompare x y == EQ

instance Ord v => Ord (SOPed v) where
  compare (SOPed (pa :: Proxy a) x) (SOPed (pb :: Proxy b) y) = case sopEqT pa pb of
    Left ord -> ord
    Right Refl -> sopCompare x y

npCompare :: Ord v => SOP.NP (SOP.K v) a' -> SOP.NP (SOP.K v) a' -> Ordering
npCompare (x SOP.:* xs) (y SOP.:* ys) = case compare x y of
  EQ -> npCompare xs ys
  x -> x
npCompare SOP.Nil SOP.Nil = EQ

sopCompare :: Ord v => SOP (SOP.K v) a -> SOP (SOP.K v) a -> Ordering
sopCompare (SOP.SOP (SOP.Z x)) (SOP.SOP (SOP.Z y)) = npCompare x y
sopCompare (SOP.SOP (SOP.S x)) (SOP.SOP (SOP.S y)) = sopCompare (SOP.SOP x) (SOP.SOP y)
sopCompare (SOP.SOP (SOP.Z _)) (SOP.SOP (SOP.S _)) = LT
sopCompare (SOP.SOP (SOP.S _)) (SOP.SOP (SOP.Z _)) = GT

sopEqT :: forall a b. (PGeneric a, PGeneric b) => Proxy a -> Proxy b -> Either Ordering (a :~: b)
sopEqT _ _ =
  let infoa = SOP.gdatatypeInfo (Proxy @(PConcrete EK a))
      infob = SOP.gdatatypeInfo (Proxy @(PConcrete EK b))
      moda = SOP.moduleName infoa
      modb = SOP.moduleName infob
      nma = SOP.datatypeName infoa
      nmb = SOP.datatypeName infob
   in if moda == modb && nma == nmb
        then Right $ unsafeCoerce Refl
        else Left $ compare (moda, nma) (modb, nmb)

-- | The evaluation value. Non-normal forms are excluded.
data Val
  = VLam Lam -- \x. M
  | VPair (Val, Val) -- (x, y)
  | VEither (Either Val Val) -- Left x / Right y
  | VInt Integer -- 123
  | VBS BS -- "abc"
  | VNat Natural
  | VList (List Val) -- [a, b, c]
  | VUnit -- ()
  | VSOP (SOPed Val)
  | VPubKeyHash PubKeyHash
  | VCurrencySymbol CurrencySymbol
  | VTokenName TokenName
  | VAddress Address
  | VValue Value
  | VData Data
  | VUTXORef UTXORef
  | VUTXO UTXO
  | VOwnUTXO OwnUTXO
  | VDiagram Diagram
  | VTimeRange TimeRange
  | VNeutral Neutral
  deriving stock (Eq, Ord)

data Pat
  = -- Either
    MLeft Ident
  | MRight Ident
  | -- Natural
    MNZ
  | MNS Ident
  | -- List
    MLNil
  | MLCons Ident Ident
  | -- Data
    MDataConstr Ident Ident
  | MDataMap Ident
  | MDataList Ident
  | MDataInt Ident
  | MDataBS Ident
  | -- SOP
    MSOP (SOPed Ident)
  deriving stock (Eq, Ord)

-- | Type of values.
data Ty
  = TFun Ty Ty -- A -> B
  | TProd Ty Ty -- (A, B)
  | TSum Ty Ty -- A | B
  | TInt -- Integer
  | TBS -- ByteString
  | TNat -- Natural
  | TList Ty -- [A]
  | TUnit -- ()
  | forall a. PGeneric a => TSOP (Proxy a)
  | TPubKeyHash
  | TCurrencySymbol
  | TTokenName
  | TAddress
  | TValue
  | TData
  | TUTXORef
  | TUTXO
  | TOwnUTXO Ty
  | TDiagram Ty
  | TTimeRange

vdec :: Neutral -> [(Pat, Val)] -> Val
vdec scrut tree = VNeutral $ NDec $ Decision scrut tree

vid :: Ident -> Val
vid = VNeutral . NId

vproj :: Neutral -> Text -> Val
vproj re field = VNeutral $ NProj $ Projection re field

vapp :: Neutral -> Val -> Val
vapp (NSp (Spine hd xs)) x = VNeutral $ NSp (Spine hd (xs ++ [x]))
vapp neu x = VNeutral $ NSp (Spine neu [x])

pprim :: PrimOp -> Partial a
pprim = PNeutral . NPrim

toVal :: (a -> Val) -> Partial a -> Val
toVal _ (PNeutral x) = VNeutral x
toVal f (PNormal x) = f x

-- evalProj :: Val -> Text -> Partial Val
-- evalProj = _

-- evalApp :: Val -> [Val] -> Partial Val
-- evalApp = _

-- nsubst :: (Ident -> Partial Val) -> Neutral -> Partial Val
-- nsubst resolve = \case
--   NId ident -> resolve ident
--   NProj (Projection re field) -> case nsubst resolve re of
--     PNormal val -> evalProj val field
--     PNeutral re' -> PNeutral $ NProj (Projection re' field)
--   NSp (Spine f xs) -> case nsubst resolve f of
--     PNormal val -> evalApp val xs
--     PNeutral f' -> PNeutral $ NSp (Spine re' field)

--------------------------------------------------------------------------------
-- Transaction types -----------------------------------------------------------
--------------------------------------------------------------------------------

toHex :: ByteString -> String
toHex bs = do
  x <- BS.unpack bs
  fmap (intToDigit . fromIntegral) [x `div` 16, x `mod` 16]

prettyHex :: ByteString -> Doc a
prettyHex bs = pretty $ take 7 $ toHex bs

data Value = Value
  { valueAda :: Partial Integer
  , valueOwn :: Map (Partial TokenName) (Partial Integer)
  , valueOther :: Map (Partial CurrencySymbol) (Map (Partial TokenName) (Partial Integer))
  }
  deriving stock (Eq, Ord)

instance Semigroup Value where
  Value a o xs <> Value a' o' ys =
    Value
      (a + a')
      (Map.filter (/= 0) $ Map.unionWith (+) o o')
      ( Map.filter Map.null $
          Map.map (Map.filter (/= 0)) $
            Map.unionWith (Map.unionWith (+)) xs ys
      )

instance Monoid Value where
  mempty = Value 0 Map.empty Map.empty

newtype PubKeyHash = PubKeyHash ByteString
  deriving stock (Eq, Ord)

newtype CurrencySymbol = CurrencySymbol ByteString
  deriving stock (Eq, Ord)

newtype TokenName = TokenName BS
  deriving stock (Eq, Ord)

newtype Address = AddrPubKey (Partial PubKeyHash)
  deriving stock (Eq, Ord)

newtype BS = BS ByteString
  deriving stock (Eq, Ord)
  deriving (Semigroup, Monoid, IsString) via ByteString

data Data
  = DataConstr (Partial Integer) (Partial (List (Partial Data)))
  | DataMap (Partial (List (Partial (Partial Data, Partial Data))))
  | DataList (Partial (List (Partial Data)))
  | DataInt (Partial Integer)
  | DataBS (Partial BS)
  deriving stock (Eq, Ord)

newtype UTXORef = UTXORef ByteString
  deriving stock (Eq, Ord)

-- | Diagram backend's representation of a non-own UTXO.
data UTXO
  = -- | UTXO from an address.
    UTXOAddr (Partial Address) (Partial Value) (Partial Data)
  | -- | UTXO from a script belonging to a certain protocol.
    UTXOProtocol String (Partial Value) Val
  deriving stock (Eq, Ord)

-- | UTXO of the current protocol.
data OwnUTXO = OwnUTXO (Partial Value) Val
  deriving stock (Eq, Ord)

-- | Transaction outputs.
data Output
  = -- | The output must happen, but need not be unique.
    OutputWitness (Partial UTXO)
  | -- | The output must happen, and must be unique.
    OutputCreate (Partial UTXO)
  | OutputOwn (Partial OwnUTXO)
  deriving stock (Eq, Ord)

newtype EvalTime = EvalTime POSIXTime
  deriving stock (Eq, Ord)

type TimeRange = Interval EvalTime

-- | A transaction diagram.
data Diagram = Diagram
  { diagOwnInputs :: [Partial OwnUTXO]
  -- ^ Input from the current protocol.
  , diagInputs :: Set (Partial UTXORef)
  -- ^ References to inputs from other protocols.
  , diagMap :: [(Partial UTXORef, Partial UTXO)]
  -- ^ Mapping from UTXO references to actual UTXOs. 1-N relationships are bogus.
  , diagOutputs :: [Partial Output]
  -- ^ Outputs of the transaction.
  , diagSign :: Set (Partial PubKeyHash)
  -- ^ Signatories of the transaction.
  , diagMint :: Partial Value
  -- ^ Minted value of the transaction.
  , diagTimeRange :: Partial TimeRange
  -- ^ The time range in which the transaction should be valid.
  }
  deriving stock (Eq, Ord)

partialIvIntersection :: Partial TimeRange -> Partial TimeRange -> Partial TimeRange
partialIvIntersection = binPrim Iv.intersection PrimIvIntersection VTimeRange

instance Semigroup Diagram where
  Diagram ownIns ins inMap outs sign mint tr <> Diagram ownIns' ins' inMap' outs' sign' mint' tr' =
    Diagram
      (ownIns <> ownIns')
      (ins <> ins')
      (inMap <> inMap')
      (outs <> outs')
      (sign <> sign')
      (mint <> mint')
      (tr `partialIvIntersection` tr')

instance Monoid Diagram where
  mempty = Diagram mempty mempty mempty mempty mempty (PNormal mempty) (PNormal Iv.always)

--------------------------------------------------------------------------------
-- Conversion helpers ----------------------------------------------------------
--------------------------------------------------------------------------------

mkConvHelper "Pair" [t|(Val, Val)|]
mkConvHelper "List" [t|List Val|]
mkConvHelper "Nat" [t|Natural|]
mkConvHelper "Int" [t|Integer|]
mkConvHelper' "BS"
mkConvHelper' "Data"
mkConvHelper' "Value"
mkConvHelper' "Diagram"
mkConvHelper' "UTXORef"
mkConvHelper' "UTXO"
mkConvHelper' "OwnUTXO"
mkConvHelper' "CurrencySymbol"
mkConvHelper' "TokenName"
mkConvHelper' "PubKeyHash"
mkConvHelper' "Address"
mkConvHelper' "TimeRange"

--------------------------------------------------------------------------------
-- Runtime type information ----------------------------------------------------
--------------------------------------------------------------------------------

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (PEither a b) where
  typeReprInfo _ = TSum (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (PPair a b) where
  typeReprInfo _ = TProd (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (a #-> b) where
  typeReprInfo _ = TFun (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance TypeInfo a => TypeReprInfo (PList a) where
  typeReprInfo _ = TList (typeInfo (Proxy @a))

instance PGeneric a => TypeReprInfo (PSOPed a) where
  typeReprInfo _ = TSOP (Proxy @a)

instance TypeInfo d => TypeReprInfo (POwnUTXO d) where
  typeReprInfo _ = TOwnUTXO (typeInfo (Proxy @d))

instance TypeInfo d => TypeReprInfo (PDiagram d) where
  typeReprInfo _ = TDiagram (typeInfo (Proxy @d))

instance TypeReprInfo PUnit where typeReprInfo _ = TUnit
instance TypeReprInfo PInteger where typeReprInfo _ = TInt
instance TypeReprInfo PByteString where typeReprInfo _ = TBS
instance TypeReprInfo PNat where typeReprInfo _ = TNat
instance TypeReprInfo PPubKeyHash where typeReprInfo _ = TPubKeyHash
instance TypeReprInfo PCurrencySymbol where typeReprInfo _ = TCurrencySymbol
instance TypeReprInfo PTokenName where typeReprInfo _ = TTokenName
instance TypeReprInfo PAddress where typeReprInfo _ = TAddress
instance TypeReprInfo PValue where typeReprInfo _ = TValue
instance TypeReprInfo PData where typeReprInfo _ = TData
instance TypeReprInfo PUTXO where typeReprInfo _ = TUTXO
instance TypeReprInfo PUTXORef where typeReprInfo _ = TUTXORef
instance TypeReprInfo PTimeRange where typeReprInfo _ = TTimeRange

--------------------------------------------------------------------------------
-- Construction and elimination ------------------------------------------------
--------------------------------------------------------------------------------

instance PConstructablePrim EK PUnit where
  pconImpl _ = Eval $ pure VUnit
  pmatchImpl _ f = f PUnit
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructablePrim EK (PPair a b)
  where
  pconImpl (PPair (MkTerm x) (MkTerm y)) = Eval $ VPair <$> ((,) <$> x <*> y)
  pmatchImpl (Eval prod) f = term do
    prod >>= \case
      VPair (x, y) -> runTerm $ f $ PPair (pterm x) (pterm y)
      VNeutral neu -> runTerm $ f $ PPair (pterm $ vproj neu "fst") (pterm $ vproj neu "snd")
      _ -> unsound "Pair"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructablePrim EK (PEither a b)
  where
  pconImpl (PLeft (MkTerm x)) = Eval $ VEither . Left <$> x
  pconImpl (PRight (MkTerm x)) = Eval $ VEither . Right <$> x
  pmatchImpl (Eval sum) f = term do
    sum >>= \case
      VEither (Left l) -> runTerm $ f $ PLeft (pterm l)
      VEither (Right r) -> runTerm $ f $ PRight (pterm r)
      VNeutral neu -> do
        caseLeft <- withIdent "l" \l -> (MLeft l,) <$> runTerm (f $ PLeft $ pterm $ vid l)
        caseRight <- withIdent "r" \r -> (MRight r,) <$> runTerm (f $ PRight $ pterm $ vid r)
        pure $ vdec neu [caseLeft, caseRight]
      _ -> unsound "Either"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructablePrim EK (a #-> b)
  where
  pconImpl (PLam f) = Eval do
    uid <- freshUid
    run <- takeEvalM
    pure $ VLam $ Lam uid $ \x -> run $ runTerm $ f $ pterm x
  pmatchImpl (Eval lam) f = term do
    lam >>= \case
      VLam (Lam _ lam') -> runTerm $ f $ PLam \(MkTerm x) -> term $ lam' <$> x
      VNeutral neu -> runTerm $ f $ PLam \(MkTerm x) -> term $ vapp neu <$> x
      _ -> unsound "Lambda"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance PConstructablePrim EK PNat where
  pconImpl PZ = Eval $ pure $ VNat NZ
  pconImpl (PS (MkTerm n)) = Eval $ VNat . NS <$> fmap intoNat n
  pmatchImpl (Eval n) f = term do
    n >>= \case
      VNat n' -> case n' of
        NZ -> runTerm $ f PZ
        NS pn' -> runTerm $ f $ PS (pterm $ toVal VNat pn')
      VNeutral neu -> do
        caseZero <- (MNZ,) <$> runTerm (f PZ)
        caseSuc <- withIdent "n" \n ->
          (MNS n,) <$> runTerm (f $ PS $ pterm $ vid n)
        pure $ vdec neu [caseZero, caseSuc]
      _ -> unsound "Natural"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance TypeInfo a => PConstructablePrim EK (PList a) where
  pconImpl PNil = Eval $ pure $ VList LNil
  pconImpl (PCons (MkTerm x) (MkTerm xs)) =
    Eval $ fmap VList $ LCons <$> x <*> (intoList <$> xs)
  pmatchImpl (Eval n) f = term do
    n >>= \case
      VList xs' -> case xs' of
        LNil -> runTerm $ f PNil
        LCons x' pxs' -> runTerm $ f $ PCons (pterm x') (pterm $ toVal VList pxs')
      VNeutral neu -> do
        caseNil <- (MLNil,) <$> runTerm (f PNil)
        caseCons <- withIdent "x" \x -> withIdent "xs" \xs ->
          (MLCons x xs,) <$> runTerm (f $ PCons (pterm $ vid x) (pterm $ vid xs))
        pure $ vdec neu [caseNil, caseCons]
      _ -> unsound "List"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

newtype WithIdents xs = WithIdents (forall a. (NP (SOP.K Ident) xs -> EvalM a) -> EvalM a)

data HTup f g xs = HTup (f xs) (g xs)

instance
  PGeneric a =>
  PConstructablePrim EK (PSOPed a)
  where
  pconImpl (PSOPed x) = Eval do
    gx <-
      traverse'_SOP (fmap SOP.K . SOP.unK) $
        map_SOP (\(Pf' x) -> SOP.K $ runTerm x) $
          pgfrom (Proxy @a) (Proxy @(PConcreteEf EK)) $
            SOP.gfrom x
    pure $ VSOP $ SOPed (Proxy @a) gx
  pmatchImpl (Eval x) f = term do
    x >>= \case
      VSOP (SOPed _ x') -> do
        let unGx =
              SOP.gto $
                pgto (Proxy @a) (Proxy @(PConcreteEf EK)) $
                  map_SOP (Pf' . pterm . SOP.unK) $
                    -- it MUST be this particular type, but we can't possibly know
                    unsafeCoerce @(SOP (SOP.K Val) _) @(SOP (SOP.K Val) (PCode a)) x'
        runTerm $ f $ PSOPed unGx
      VNeutral neu -> do
        let injs = injections @(PCode a)
        let info = map_NP sopFieldNames $ SOP.constructorInfo $ SOP.gdatatypeInfo (Proxy @(a (PConcreteEf EK)))
        let zipped = zipWith_NP HTup injs (unsafeCoerce info)
        x <- flip
          (ctraverse'_NP (Proxy @SOP.SListI))
          zipped
          \(HTup (SOP.Fn inj :: Injection (NP (Pf' (PConcreteEf EK))) (PCode a) xs) fields) -> do
            let
              (WithIdents withIdents) =
                cata_NP @WithIdents
                  (WithIdents \f -> f SOP.Nil)
                  (\(SOP.K name) (WithIdents wi) -> WithIdents \f -> withIdent (T.pack name) \x -> wi \xs -> f (SOP.K x SOP.:* xs))
                  fields
            withIdents \ids -> do
              val <-
                runTerm . f . PSOPed . SOP.gto . pgto (Proxy @a) (Proxy @(PConcreteEf EK)) . SOP.SOP . SOP.unK . inj $
                  map_NP (\(SOP.K id) -> Pf' $ pterm $ vid id) ids
              pure $ SOPChoice ids val
        pure $ VNeutral $ NDecSOP $ SOPDec (Proxy @a) neu x
      _ -> unsound "SOP"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

sopFieldNames :: SOP.ConstructorInfo xs -> NP (SOP.K String) xs
sopFieldNames = \case
  SOP.Constructor {} -> pure_NP (SOP.K "x")
  SOP.Infix {} -> SOP.K "lhs" SOP.:* SOP.K "rhs" SOP.:* SOP.Nil
  SOP.Record _ info -> map_NP (SOP.K . SOP.fieldName) info

instance IsPType EK d => PConstructablePrim EK (POwnUTXO d) where
  pconImpl (POwnUTXO (MkTerm val) (MkTerm dat)) =
    Eval $ fmap VOwnUTXO $ OwnUTXO <$> (intoValue <$> val) <*> dat
  pmatchImpl (Eval x) f = term do
    x >>= \case
      VOwnUTXO (OwnUTXO val dat) -> runTerm $ f $ POwnUTXO (pterm $ toVal VValue val) (pterm dat)
      VNeutral neu -> runTerm . f $ POwnUTXO (pterm $ vproj neu "value") (pterm $ vproj neu "datum")
      _ -> unsound "OwnUTXO"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance PConstructablePrim EK PData where
  pconImpl x = Eval $ fmap VData case x of
    PDataConstr (MkTerm var) (MkTerm vals) ->
      DataConstr <$> (intoInt <$> var) <*> (fmap (fmap intoData) . intoList <$> vals)
    PDataMap (MkTerm xs) ->
      DataMap <$> (fmap (fmap $ fmap (bimap intoData intoData) . intoPair) . intoList <$> xs)
    PDataList (MkTerm xs) ->
      DataList <$> (fmap (fmap intoData) . intoList <$> xs)
    PDataInteger (MkTerm n) ->
      DataInt <$> (intoInt <$> n)
    PDataByteString (MkTerm bs) ->
      DataBS <$> (intoBS <$> bs)
  pmatchImpl (Eval x) f = term do
    x >>= \case
      VData d -> case d of
        DataConstr var vals -> runTerm $ f $ PDataConstr (pterm $ toVal VInt var) (pterm $ toVal VList $ fmap (fmap (toVal VData)) vals)
        DataMap xs -> runTerm $ f $ PDataMap (pterm $ toVal VList $ fmap (fmap $ toVal $ VPair . bimap (toVal VData) (toVal VData)) xs)
        DataList xs -> runTerm $ f $ PDataList (pterm $ toVal VList $ fmap (fmap $ toVal VData) xs)
        DataInt n -> runTerm $ f $ PDataInteger (pterm $ toVal VInt n)
        DataBS bs -> runTerm $ f $ PDataByteString (pterm $ toVal VBS bs)
      VNeutral neu -> do
        caseConstr <- withIdent "variant" \var -> withIdent "args" \args ->
          (MDataConstr var args,) <$> runTerm (f $ PDataConstr (pterm $ vid var) (pterm $ vid args))
        caseMap <- withIdent "map" \xs ->
          (MDataMap xs,) <$> runTerm (f $ PDataMap (pterm $ vid xs))
        caseList <- withIdent "xs" \xs ->
          (MDataList xs,) <$> runTerm (f $ PDataList (pterm $ vid xs))
        caseInt <- withIdent "n" \n ->
          (MDataInt n,) <$> runTerm (f $ PDataInteger (pterm $ vid n))
        caseBS <- withIdent "bs" \bs ->
          (MDataBS bs,) <$> runTerm (f $ PDataByteString (pterm $ vid bs))
        pure $ vdec neu [caseConstr, caseMap, caseList, caseInt, caseBS]
      _ -> unsound "Data"
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

--------------------------------------------------------------------------------
-- Other typeclass support -----------------------------------------------------
--------------------------------------------------------------------------------

instance Num (Term EK PInteger) where
  MkTerm x + MkTerm y = term $ toVal VInt <$> liftA2 (+) (intoInt <$> x) (intoInt <$> y)
  MkTerm x - MkTerm y = term $ toVal VInt <$> liftA2 (-) (intoInt <$> x) (intoInt <$> y)
  MkTerm x * MkTerm y = term $ toVal VInt <$> liftA2 (*) (intoInt <$> x) (intoInt <$> y)
  abs (MkTerm x) = term $ toVal VInt . abs . intoInt <$> x
  signum (MkTerm x) = term $ toVal VInt . signum . intoInt <$> x
  fromInteger n = pterm $ VInt n

instance IsString (Term EK PByteString) where
  fromString xs = pterm $ VBS $ BS $ T.encodeUtf8 $ T.pack xs

instance Semigroup (Term EK PByteString) where
  MkTerm x <> MkTerm y = term $ toVal VBS <$> liftA2 (<>) (intoBS <$> x) (intoBS <$> y)

instance Monoid (Term EK PByteString) where
  mempty = pterm $ VBS $ BS BS.empty

instance Semigroup (Term EK PValue) where
  MkTerm x <> MkTerm y = term $ toVal VValue <$> liftA2 (<>) (intoValue <$> x) (intoValue <$> y)

instance Monoid (Term EK PValue) where
  mempty = pterm $ VValue mempty

instance Semigroup (Term EK (PDiagram d)) where
  MkTerm x <> MkTerm y = term $ toVal VDiagram <$> liftA2 (<>) (intoDiagram <$> x) (intoDiagram <$> y)

instance Monoid (Term EK (PDiagram d)) where
  mempty = pterm $ VDiagram mempty

--------------------------------------------------------------------------------
-- Speccing support ------------------------------------------------------------
--------------------------------------------------------------------------------

instance PPSL EK where
  requireInput (MkTerm ref) = term do
    ref' <- Set.singleton <$> fmap intoUTXORef ref
    pure $ VDiagram $ mempty {diagInputs = ref'}
  requireOwnInput (MkTerm utxo) = term do
    utxo' <- intoOwnUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOwnInputs = [utxo']}
  createOwnOutput (MkTerm utxo) = term do
    utxo' <- intoOwnUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [PNormal $ OutputOwn utxo']}
  witnessOutput (MkTerm utxo) = term do
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [PNormal $ OutputWitness utxo']}
  createOutput (MkTerm utxo) = term do
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [PNormal $ OutputCreate utxo']}
  mintOwn (MkTerm tn) (MkTerm n) = term do
    val <- Map.singleton <$> (intoTokenName <$> tn) <*> (intoInt <$> n)
    pure $ VDiagram $ mempty {diagMint = PNormal $ mempty {valueOwn = val}}
  witnessMint (MkTerm cs) (MkTerm tn) (MkTerm n) = term do
    val <- Map.singleton <$> (intoCurrencySymbol <$> cs) <*> (Map.singleton <$> (intoTokenName <$> tn) <*> (intoInt <$> n))
    pure $ VDiagram $ mempty {diagMint = PNormal $ mempty {valueOther = val}}
  requireSignature (MkTerm pkh) = term do
    pkh' <- intoPubKeyHash <$> pkh
    pure $ VDiagram $ mempty {diagSign = Set.singleton pkh'}
  requireValidRange (MkTerm range) = term do
    range' <- intoTimeRange <$> range
    pure $ VDiagram $ mempty {diagTimeRange = range'}
  toAddress (MkTerm addr) (MkTerm val) (MkTerm dat) =
    term $ fmap VUTXO $ UTXOAddr <$> (intoAddress <$> addr) <*> (intoValue <$> val) <*> (intoData <$> dat)
  toProtocol p (MkTerm dat) (MkTerm val) =
    term $ fmap VUTXO $ UTXOProtocol (protocolName p) <$> (intoValue <$> val) <*> dat
  fromPkh (MkTerm pkh) =
    term $ fmap VAddress $ AddrPubKey <$> (intoPubKeyHash <$> pkh)
  utxoRefIs (MkTerm ref) (MkTerm utxo) = term do
    ref' <- intoUTXORef <$> ref
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagMap = [(ref', utxo')]}
  emptyValue = pterm $ VValue mempty
  mkValue (MkTerm cs) (MkTerm tn) (MkTerm n) = term do
    val <- Map.singleton <$> (intoCurrencySymbol <$> cs) <*> (Map.singleton <$> (intoTokenName <$> tn) <*> (intoInt <$> n))
    pure $ VValue $ mempty {valueOther = val}
  mkAda (MkTerm n) = term do
    n' <- intoInt <$> n
    pure $ VValue $ mempty {valueAda = n'}
  mkOwnValue (MkTerm tn) (MkTerm n) = term do
    tn' <- intoTokenName <$> tn
    n' <- intoInt <$> n
    pure $ VValue $ mempty {valueOwn = Map.singleton tn' n'}

--------------------------------------------------------------------------------
-- AST prettyprinting ----------------------------------------------------------
--------------------------------------------------------------------------------

prettyChain :: [Doc a] -> Doc a
prettyChain = P.parens . P.fillSep

prettyCon :: Doc a -> [Doc a] -> Doc a
prettyCon con args = P.group . P.hang 2 . P.parens $ con <> P.line <> P.sep args

-- | @{k1: v1, k2: v2, ...}@
prettyDict :: [(Doc a, Doc a)] -> Doc a
prettyDict =
  P.group
    . P.encloseSep (P.flatAlt "{ " "{") (P.flatAlt " }" "}") ", "
    . fmap (\(k, v) -> P.group $ P.hang 2 (k <> ":" <> P.line <> v))

{-
\${ ADA: n
  , OWN: {$(t1): n, $(t2): n, ...}
  , cs: {$(t1): n, $(t2): n, ...}
  , ...
  }
-}
instance Pretty Value where
  pretty (Value a os xss) =
    let pmap = prettyDict . fmap (bimap pretty pretty)
        other = fmap (bimap pretty (pmap . Map.toList)) . Map.toList $ xss
        own = [("OWN", pmap . Map.toList $ os) | not $ Map.null os]
        ada = [("ADA", pretty a) | a /= 0]
     in "$" <> prettyDict (ada <> own <> other)

instance Pretty Data where
  pretty = \case
    DataConstr n x -> prettyCon ("@" <> P.parens (pretty n)) [pretty x] -- (@(1) v)
    DataMap xs -> "@@" <> pretty xs -- @{k: v, ...}
    DataList xs -> "@" <> pretty xs -- @[a, b, ...]
    DataInt n -> "@" <> pretty n -- @123
    DataBS bs -> "@" <> P.dquotes (pretty bs) -- @"abc"

-- PK#123abcf
instance Pretty PubKeyHash where
  pretty (PubKeyHash x) = "PK#" <> prettyHex x

-- CS#123abcf
instance Pretty CurrencySymbol where
  pretty (CurrencySymbol x) = "CS#" <> prettyHex x

-- \$(tok name)
instance Pretty TokenName where
  pretty (TokenName x) = "$" <> P.parens (pretty x)

instance Pretty Address where
  pretty (AddrPubKey pkh) = pretty pkh

-- UTXO#123abcf
instance Pretty UTXORef where
  pretty (UTXORef x) = "UTXO#" <> prettyHex x

instance Pretty EvalTime where
  pretty (EvalTime t) = pretty $ Time.iso8601Show $ posixSecondsToUTCTime t

-- Address: (UTXOAddr PK#123abcf ${...} @...)
-- Protocol: (UTXOProtocol "protocol name" @{...} ...)
instance Pretty UTXO where
  pretty (UTXOAddr addr val dat) = prettyCon "UTXOAddr" [pretty addr, pretty val, pretty dat]
  pretty (UTXOProtocol name val dat) = prettyCon "UTXOProtocol" [P.dquotes (pretty name), pretty val, pretty dat]

instance Pretty OwnUTXO where
  pretty (OwnUTXO val dat) = prettyCon "OwnUTXO" [pretty val, pretty dat]

instance Pretty Output where
  pretty (OutputWitness utxo) = prettyCon "OutputWitness" [pretty utxo]
  pretty (OutputCreate utxo) = prettyCon "OutputCreate" [pretty utxo]
  pretty (OutputOwn utxo) = prettyCon "OutputOwn" [pretty utxo]

instance Pretty Diagram where
  pretty (Diagram ownIns ins inMap outs sign mint tr) =
    prettyCon
      "Diagram"
      [ prettyDict
          [ ("ownInputs", pretty ownIns)
          , ("inputs", pretty $ Set.toList ins)
          , ("inputMap", prettyDict $ fmap (bimap pretty pretty) inMap)
          , ("outputs", pretty outs)
          , ("signatories", pretty $ Set.toList sign)
          , ("mint", pretty mint)
          , ("validRange", pretty tr)
          ]
      ]

instance Pretty Ty where
  pretty = \case
    TFun a b -> prettyChain [pretty a, "->", pretty b]
    TProd a b -> prettyChain [pretty a <> ",", pretty b]
    TSum a b -> prettyChain [pretty a, "|", pretty b]
    TInt -> "Integer"
    TBS -> "ByteString"
    TNat -> "Natural"
    TUnit -> "()"
    TList a -> P.brackets (pretty a)
    TSOP (_ :: Proxy a) -> pretty $ SOP.datatypeName $ SOP.gdatatypeInfo (Proxy @(PConcrete EK a))
    TPubKeyHash -> "PubKeyHash"
    TCurrencySymbol -> "CurrencySymbol"
    TTokenName -> "TokenName"
    TAddress -> "Address"
    TValue -> "Value"
    TData -> "Data"
    TUTXORef -> "UTXORef"
    TUTXO -> "UTXO"
    TOwnUTXO d -> prettyCon "OwnUTXO" [pretty d]
    TDiagram d -> prettyCon "Diagram" [pretty d]
    TTimeRange -> "TimeRange"

instance Pretty BS where
  pretty (BS bs) = pretty $ T.unpack $ T.decodeUtf8 bs

instance Pretty Natural where
  pretty = prettyNat (0 :: Integer)
    where
      prettyNat n NZ = pretty n
      prettyNat n (NS (PNormal rest)) = prettyNat (n + 1) rest
      prettyNat n (NS (PNeutral neu)) = P.parens $ pretty (n + 1) <+> "+" <+> pretty neu

instance Pretty a => Pretty (List a) where
  pretty = prettyList []
    where
      prettyList xs LNil = pretty $ reverse xs
      prettyList xs (LCons x (PNormal rest)) = prettyList (x : xs) rest
      prettyList xs (LCons x (PNeutral neu)) = P.parens $ pretty (x : xs) <+> "++" <+> pretty neu

conNames :: forall a. PGeneric a => Proxy a -> [String]
conNames _ =
  collapse_NP
    ( map_NP (SOP.K . SOP.constructorName) $
        SOP.constructorInfo $
          SOP.gdatatypeInfo $
            Proxy @(PConcrete EK a)
    )

instance Pretty Val where
  pretty = \case
    VLam (Lam uid f) -> P.hang 2 . P.group . P.parens $ "λ" <> pretty uid <+> "->" <> P.line <> pretty (f $ vid $ uidToIdent uid)
    VPair (x, y) -> prettyChain [pretty x <> ",", pretty y]
    VEither et -> case et of
      Left x -> prettyCon "Left" [pretty x]
      Right y -> prettyCon "Right" [pretty y]
    VInt n -> pretty n
    VBS bs -> P.dquotes $ pretty bs
    VUnit -> "()"
    VNat n -> pretty n
    VList xs -> pretty xs
    VSOP (SOPed (pa :: Proxy a) x) ->
      let conName = conNames pa !! index_SOP x
          xs = pretty <$> collapse_SOP x
       in if null xs
            then pretty conName
            else prettyCon (pretty conName) xs
    VPubKeyHash pkh -> pretty pkh
    VCurrencySymbol cs -> pretty cs
    VTokenName nm -> pretty nm
    VAddress addr -> pretty addr
    VValue val -> pretty val
    VData dt -> pretty dt
    VUTXORef ref -> pretty ref
    VTimeRange range -> pretty range
    VUTXO utxo -> pretty utxo
    VOwnUTXO utxo -> pretty utxo
    VDiagram diag -> pretty diag
    VNeutral neu -> pretty neu

instance Pretty Ident where
  pretty (Ident 0 nm) = pretty nm
  pretty (Ident ix nm) = pretty nm <> pretty ix
  pretty (IdentM n) = "?" <> pretty n
  pretty (IdentU n) = "%" <> pretty n

instance Pretty a => Pretty (Partial a) where
  pretty = \case
    PNormal a -> pretty a
    PNeutral neu -> pretty neu

instance Pretty Pat where
  pretty =
    P.hsep . \case
      MLeft x -> ["Left", pretty x]
      MRight x -> ["Right", pretty x]
      MNZ -> ["0"]
      MNS x -> ["1 +", pretty x]
      MLNil -> ["Nil"]
      MLCons x y -> [pretty x, ":", pretty y]
      MDataConstr x y -> ["@Constr", pretty x, pretty y]
      MDataMap x -> ["@Map", pretty x]
      MDataList x -> ["@List", pretty x]
      MDataInt x -> ["@Int", pretty x]
      MDataBS x -> ["@BS", pretty x]
      MSOP (SOPed (pa :: Proxy a) sop@(SOP.SOP ns)) ->
        let
          conName = conNames pa !! index_NS ns
          idents = collapse_SOP sop
         in
          pretty conName : fmap pretty idents

instance Pretty PrimOp where
  pretty =
    prettyChain . \case
      PrimAdd a b -> [pretty a, "+", pretty b]
      PrimSub a b -> [pretty a, "-", pretty b]
      PrimMul a b -> [pretty a, "*", pretty b]
      PrimNeg a -> ["negate", pretty a]
      PrimSignum a -> ["signum", pretty a]
      PrimAbs a -> ["abs", pretty a]
      PrimConcat a b -> [pretty a, "<>", pretty b]
      PrimIvIntersection a b -> [pretty a, "/\\", pretty b]

instance Pretty Neutral where
  pretty = \case
    NId ident -> pretty ident
    NSp (Spine hd args) -> prettyCon (pretty hd) (pretty <$> args)
    NProj (Projection re field) -> pretty re <> "." <> pretty field
    NPrim op -> pretty op
    NDec (Decision scrut match) ->
      prettyCaseExpr (pretty scrut) (fmap (bimap pretty pretty) match)
    NDecSOP (SOPDec (pa :: Proxy a) scrut match) ->
      let match' = zip (conNames pa) $ collapse_NP $ map_NP (\(SOPChoice idents val) -> SOP.K (collapse_NP idents, val)) match
       in prettyCaseExpr (pretty scrut) (fmap (\(ctor, (args, val)) -> (pretty ctor <+> P.hsep (fmap pretty args), pretty val)) match')

prettyCaseExpr :: Doc a -> [(Doc a, Doc a)] -> Doc a
prettyCaseExpr scrut match =
  P.hang 2 . P.parens $
    "case"
      <+> scrut
      <+> "of"
        <> P.line
        <> P.vsep
          ( fmap
              ( \(pat, val) ->
                  P.group . P.hang 2 $ pat <+> "->" <> P.line <> val
              )
              match
          )

instance Pretty Uid where
  pretty (Uid n) = pretty n

--------------------------------------------------------------------------------
-- Compilation -----------------------------------------------------------------
--------------------------------------------------------------------------------

compile :: forall a. IsPType EK a => Term EK a -> (Val, Ty)
compile v =
  let val = runEvalM $ runTerm v
      ty = typeInfo (Proxy @a)
   in (val, ty)

compileShow :: IsPType EK a => Term EK a -> Text
compileShow = docToText . pretty . fst . compile

docToText :: Doc a -> Text
docToText = P.renderStrict . P.layoutPretty P.defaultLayoutOptions {P.layoutPageWidth = P.AvailablePerLine 120 1.0}

data DecisionTree a
  = ConcreteDec a
  | SplitDec Neutral (Map Pat (DecisionTree a))
  deriving stock (Eq, Ord, Functor)

intoDecisionTree :: Val -> Maybe (DecisionTree Diagram)
intoDecisionTree = \case
  VDiagram diag -> Just $ ConcreteDec diag
  VNeutral neu -> case neu of
    NDec (Decision scrut match) -> SplitDec scrut <$> traverse intoDecisionTree (Map.fromList match)
    NDecSOP (SOPDec (proxy :: Proxy a) scrut match) ->
      SplitDec scrut
        <$> traverse
          intoDecisionTree
          ( Map.fromList
              $ collapse_NP
              $ map_NP
                ( \(HTup (SOP.Fn inj) (SOPChoice idents val)) ->
                    SOP.K (MSOP (SOPed proxy $ SOP.SOP $ SOP.unK $ inj idents), val)
                )
              $ zipWith_NP HTup (injections @(PCode a)) match
          )
    _ -> Nothing
  _ -> Nothing

instance Pretty a => Pretty (DecisionTree a) where
  pretty (ConcreteDec diag) = pretty diag
  pretty (SplitDec scrut match) = prettyCaseExpr (pretty scrut) (bimap pretty pretty <$> Map.toList match)

toDecisionTree :: IsPType EK d => Term EK (PDiagram d) -> Maybe (DecisionTree Diagram)
toDecisionTree = intoDecisionTree . fst . compile
