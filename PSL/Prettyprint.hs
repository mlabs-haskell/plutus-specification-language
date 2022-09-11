{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Prettyprint where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (
  ReaderT (ReaderT, runReaderT),
  ask,
  local,
 )
import Data.Functor.Const
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint, Type)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stack (callStack)
import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP
import Generics.SOP.NP qualified as SOP
import Generics.SOP.NS qualified as SOP
import PSL
import Plutarch.Core
import Plutarch.Lam
import Plutarch.PType
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Unsafe.Coerce (unsafeCoerce)

newtype Lvl = Lvl {unLvl :: Int}
  deriving (Num, Show) via Int

data PprType
  = PprName Text [PprType]
  | PprProduct PprType PprType
  | PprSum PprType PprType
  | PprList PprType
  | PprFunc PprType PprType
  deriving stock (Show)

data PprTerm
  = PprCall Text [PprTerm] -- func a b c ...
  | PprMonCat PprTerm PprTerm -- x <> y
  | PprCons PprTerm PprTerm -- x :: y
  | PprLam PprBind PprType PprTerm -- \x : A. M
  | PprApp PprTerm PprTerm -- f x
  | PprMatch PprTerm PprBind [(Text, PprTerm)] -- match M as x for { ConA -> ...; ConB -> ... }
  | PprProj PprTerm Text -- x.field
  | PprVar PprBind -- x
  | PprTodo Text
  deriving stock (Show)

data PprBind = PprBind Lvl Text -- unique bind
  deriving stock (Show)

-- data PDummy (ef :: PTypeF)

-- instance PIsNewtype PDummy where type PIsNewtype' _ = False

type PprM m = ReaderT (Lvl, Text) m

newtype Ppr m ty = Ppr {runPpr :: PprM m PprTerm}

type PK m = 'PDSLKind (Ppr m)

type TypeReprInfo :: (Type -> Type) -> forall (a :: PType). PHs a -> Constraint
class TypeReprInfo (m :: Type -> Type) ty where
  typeReprInfo :: Proxy m -> Proxy ty -> PprType

class (IsPType (PK m) a) => TypeInfo m a

instance (IsPType (PK m) a) => TypeInfo m a

typeInfo :: forall m a. TypeInfo m a => Proxy m -> Proxy a -> PprType
typeInfo pm _ = isPType (Proxy @(PK m)) (Proxy @a) (\pa -> typeReprInfo pm pa)

instance PDSL (PK m) where
  type IsPTypeBackend (PK m) = TypeReprInfo m

instance TypeReprInfo m PUnit where
  typeReprInfo _ _ = PprName "()" []

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (PPair a b) where
  typeReprInfo _ _ = PprProduct (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @b))

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (PEither a b) where
  typeReprInfo _ _ = PprSum (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @b))

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (a #-> b) where
  typeReprInfo _ _ = PprFunc (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @b))

instance TypeInfo m a => TypeReprInfo m (PList a) where
  typeReprInfo _ _ = PprList (typeInfo (Proxy @m) (Proxy @a))

instance TypeReprInfo m PNat where
  typeReprInfo _ _ = PprName "Nat" []

instance TypeReprInfo m PPType where
  typeReprInfo _ _ = PprName "Type" []

instance PIsSOP (PK m) a => TypeReprInfo m (PSOPed a) where
  typeReprInfo _ _ = PprName (T.pack $ SOP.datatypeName $ SOP.gdatatypeInfo (Proxy @(PConcrete (PK m) a))) []

-- instance TypeReprInfo m PDummy where
--   typeReprInfo _ _ = PprDummy

-- instance (forall e. TypeInfo m e => TypeInfo m (f e)) => TypeReprInfo m (PFix f) where
--   typeReprInfo _ _ = typeInfo (Proxy @m) (Proxy @(f PDummy))

instance TypeInfo m d => TypeReprInfo m (PDiagram d) where
  typeReprInfo _ _ = PprName "Diagram" [typeInfo (Proxy @m) (Proxy @d)]

instance TypeReprInfo m PInteger where typeReprInfo _ _ = PprName "Integer" []

instance TypeReprInfo m PValue where typeReprInfo _ _ = PprName "Value" []

instance TypeReprInfo m PUTXO where typeReprInfo _ _ = PprName "UTXO" []

instance TypeReprInfo m PUTXORef where typeReprInfo _ _ = PprName "UTXORef" []

instance TypeReprInfo m PTokenName where typeReprInfo _ _ = PprName "TokenName" []

instance TypeReprInfo m PCurrencySymbol where typeReprInfo _ _ = PprName "CurrencySymbol" []

instance TypeReprInfo m PTimeRange where typeReprInfo _ _ = PprName "TimeRange" []

instance TypeReprInfo m PPubKeyHash where typeReprInfo _ _ = PprName "PubKeyHash" []

instance TypeReprInfo m PAddress where typeReprInfo _ _ = PprName "Address" []

instance TypeReprInfo m PDCert where typeReprInfo _ _ = PprName "DCert" []

instance TypeReprInfo m PData where typeReprInfo _ _ = PprName "Data" []

getCurrentVar :: Monad m => PprM m PprBind
getCurrentVar = do
  (lvl, nm) <- ask
  pure $ PprBind lvl nm

bound :: Text -> PprM m a -> PprM m a
bound nm m = local (\(l, _) -> (l + 1, nm)) m

call :: Applicative m => Text -> [PprM m PprTerm] -> PprM m PprTerm
call nm args = PprCall nm <$> sequenceA args

instance Applicative m => PConstructable' (PK m) PUnit where
  pconImpl _ = Ppr $ call "Unit" []
  pmatchImpl _ f = f PUnit

instance
  (Monad m, TypeInfo m a, TypeInfo m b) =>
  PConstructable' (PK m) (PPair a b)
  where
  pconImpl (PPair (MkTerm x) (MkTerm y)) = Ppr $ call "Pair" [x, y]
  pmatchImpl (Ppr pr) f = term do
    pr' <- pr
    bound "pair" do
      var <- getCurrentVar
      matched <- runTerm $ f $ PPair (proj var "1") (proj var "2")
      pure $ PprMatch pr' var [("Pair", matched)]

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall m a. PprM m PprTerm -> Term (PK m) a
pattern MkTerm {runTerm} <- (Term (Ppr runTerm))

term :: PprM m PprTerm -> Term (PK m) a
term x = Term (Ppr x)

pterm :: Applicative m => PprTerm -> Term (PK m) a
pterm t = term $ pure t

instance (Monad m, TypeInfo m a, TypeInfo m b) => PConstructable' (PK m) (PEither a b) where
  pconImpl (PLeft (MkTerm x)) = Ppr $ call "Left" [x]
  pconImpl (PRight (MkTerm x)) = Ppr $ call "Right" [x]
  pmatchImpl (Ppr et) f = term do
    et' <- et
    bound "either" do
      var <- getCurrentVar
      matchLeft <- runTerm $ f $ PLeft (proj var "l")
      matchRight <- runTerm $ f $ PRight (proj var "r")
      pure $ PprMatch et' var [("Left", matchLeft), ("Right", matchRight)]

instance (Monad m, TypeInfo m a, TypeInfo m b) => PConstructable' (PK m) (a #-> b) where
  pconImpl (PLam f) = Ppr $ bound "x" do
    var <- getCurrentVar
    body <- runTerm $ f $ pterm $ PprVar var
    pure $ PprLam var (typeInfo (Proxy @m) (Proxy @a)) body
  pmatchImpl (Ppr lm) f = f $ PLam \(MkTerm x) -> term $ PprApp <$> lm <*> x

-- instance (forall a. PConstructable Ppr a => PConstructable Ppr (f a))
--   => PConstructable' (Ppr m) (PFix f) where

proj :: Applicative m => PprBind -> Text -> Term (PK m) a
proj var field = pterm $ PprProj (PprVar var) field

instance Monad m => PConstructable' (PK m) PData where
  pconImpl dat = Ppr $ case dat of
    PDataConstr (MkTerm sel) (MkTerm x) -> call "[D]Con" [sel, x]
    PDataMap (MkTerm list) -> call "[D]Map" [list]
    PDataList (MkTerm list) -> call "[D]List" [list]
    PDataInteger (MkTerm int) -> call "[D]Int" [int]
    PDataByteString (MkTerm bs) -> call "[D]BS" [bs]
  pmatchImpl (Ppr dat) f = term do
    dat' <- dat
    bound "dat" do
      var <- getCurrentVar
      matchCon <- runTerm $ f $ PDataConstr (proj var "sel") (proj var "val")
      matchMap <- runTerm $ f $ PDataMap (proj var "map")
      matchList <- runTerm $ f $ PDataList (proj var "list")
      matchInt <- runTerm $ f $ PDataInteger (proj var "int")
      matchBS <- runTerm $ f $ PDataByteString (proj var "str")
      pure $ PprMatch dat' var [("[D]Con", matchCon), ("[D]Map", matchMap), ("[D]List", matchList), ("[D]Int", matchInt), ("[D]BS", matchBS)]

instance (TypeInfo m a, Monad m) => PConstructable' (PK m) (PList a) where
  pconImpl n = Ppr $ case n of
    PNil -> call "[]" []
    PCons (MkTerm x) (MkTerm xs) -> PprCons <$> x <*> xs
  pmatchImpl (Ppr n) f = term do
    n' <- n
    bound "list" do
      var <- getCurrentVar
      matchNil <- runTerm $ f $ PNil
      matchCons <- runTerm $ f $ PCons (proj var "hd") (proj var "tl")
      pure $ PprMatch n' var [("Z", matchNil), ("S", matchCons)]

instance Monad m => PConstructable' (PK m) PNat where
  pconImpl n = Ppr $ case n of
    PZ -> call "Z" []
    PS (MkTerm x) -> call "S" [x]
  pmatchImpl (Ppr n) f = term do
    n' <- n
    bound "nat" do
      var <- getCurrentVar
      matchZ <- runTerm $ f $ PZ
      matchS <- runTerm $ f $ PS (proj var "pred")
      pure $ PprMatch n' var [("Z", matchZ), ("S", matchS)]

-- TODO

instance (Monad m, PIsSOP (PK m) a) => PConstructable' (PK m) (PSOPed a) where
  pconImpl (PSOPed (x :: a f)) = case esop (Proxy @(PK m)) (Proxy @a) of
    PIsSumR _ _ from -> Ppr $ do
      let i = from $ SOP.gfrom x
      let conName = T.pack $ gsFindConName (SOP.constructorInfo $ SOP.gdatatypeInfo $ Proxy @(a f)) i
      xs <-
        sequence
          . SOP.collapse_SOP
          $ SOP.cmap_SOP (Proxy @(IsPType (PK m))) (SOP.K . runTerm) i
      pure $ PprCall conName xs
  pmatchImpl (Ppr x) (f :: PSOPed a f -> Term (PK m) b) = case esop (Proxy @(PK m)) (Proxy @a) of
    PIsSumR (_ :: Proxy inner) to _ -> term $ bound "c" do
      x' <- x
      var <- getCurrentVar
      let conNames = SOP.collapse_NP $ SOP.map_NP (SOP.K . T.pack . SOP.constructorName) $ SOP.constructorInfo $ SOP.gdatatypeInfo (Proxy @(a f))
      xs <-
        sequence
          . fmap (\x -> runTerm (f . PSOPed . SOP.gto $ to x))
          . SOP.apInjs_POP
          $ gpAllConProjs (SOP.constructorInfo . SOP.gdatatypeInfo $ Proxy @(a f)) (PprVar var)
      pure $ PprMatch x' var (zip conNames xs)

gsFindConName :: forall xss' f xss. SOP.NP SOP.ConstructorInfo xss' -> SOP.SOP f xss -> String
gsFindConName (_ SOP.:* cons) (SOP.SOP (SOP.S next)) = gsFindConName cons (SOP.SOP next)
gsFindConName (con SOP.:* _) (SOP.SOP (SOP.Z _)) = SOP.constructorName con
gsFindConName _ _ = undefined

-- data ConstEx c xss = forall xss'. ConstEx (c xss')

gpAllConProjs ::
  forall xss' m xss.
  (Applicative m, SOP.SListI2 xss', SOP.All2 (IsPType (PK m)) xss) =>
  SOP.NP SOP.ConstructorInfo xss' ->
  PprTerm ->
  SOP.POP (Term (PK m)) xss
gpAllConProjs info var =
  SOP.POP
    . SOP.cmap_NP
      (Proxy @(SOP.All (IsPType (PK m))))
      (\x -> gpAllProjs x var)
    . unsafeCoerce @(SOP.NP (SOP.NP (Const Text)) xss') @(SOP.NP (SOP.NP (Const Text)) xss)
    $ SOP.cmap_NP (Proxy @SOP.SListI) getFieldNames info

-- gpAllConProjs info var =
--   SOP.POP $
--     SOP.cana_NP
--       (Proxy @(SOP.All (IsPType (PK m))))
--       ( \(ConstEx fields) -> case fields of
--           SOP.Nil -> undefined
--           f SOP.:* fs -> (gpAllProjs f var, ConstEx fs)
--       )
--       (ConstEx $ SOP.cmap_NP (Proxy @SOP.SListI) getFieldNames info)

getFieldNames :: SOP.SListI xs => SOP.ConstructorInfo xs -> SOP.NP (Const Text) xs
getFieldNames = \case
  SOP.Record _ fs -> SOP.map_NP (\(SOP.FieldInfo nm) -> Const $ T.pack nm) fs
  _ -> SOP.ana_NP (\(Const n) -> (Const $ T.pack $ show n, Const (n + 1))) (Const (1 :: Int))

gpAllProjs ::
  forall xs' m xs.
  (Applicative m, SOP.All (IsPType (PK m)) xs) =>
  SOP.NP (Const Text) xs' ->
  PprTerm ->
  SOP.NP (Term (PK m)) xs
gpAllProjs fields var =
  SOP.cmap_NP
    (Proxy @(IsPType (PK m)))
    (\(Const f) -> pterm $ PprProj var f)
    . unsafeCoerce @(SOP.NP (Const Text) xs') @(SOP.NP (Const Text) xs)
    $ fields

-- gpAllProjs fields var =
--   SOP.cana_NP
--     (Proxy @(IsPType (PK m)))
--     ( \(ConstEx fields') -> case fields' of
--         SOP.Nil -> undefined
--         Const f SOP.:* fs -> (pterm $ PprProj var f, ConstEx fs)
--     )
--     (ConstEx fields)

instance (Applicative m) => Semigroup (Term (PK m) (PDiagram d)) where
  l <> r = term $ PprMonCat <$> runTerm l <*> runTerm r

instance (Applicative m) => Monoid (Term (PK m) (PDiagram d)) where
  mempty = pterm $ PprCall "mempty" []

instance (Applicative m) => Semigroup (Term (PK m) PValue) where
  (MkTerm l) <> (MkTerm r) = term $ PprMonCat <$> l <*> r

instance (Applicative m) => Monoid (Term (PK m) PValue) where
  mempty = pterm $ PprCall "mempty" []

instance Applicative m => PAp m (PK m) where
  papr pre x = term $ (ReaderT $ const pre) *> runTerm x
  papl x aft = term $ runTerm x <* (ReaderT $ const aft)

instance Monad m => PEmbeds m (PK m) where
  pembed m = term do
    tm <- lift m
    runTerm tm

instance Applicative m => PPartial (PK m) where
  perror = pterm $ PprCall "fail" []

compile' :: forall a m. (Applicative m, IsPType (PK m) a) => Term (PK m) a -> m (PprTerm, PprType)
compile' v = (,) <$> runReaderT (runTerm v) (-1, "") <*> pure (typeInfo (Proxy @m) (Proxy @a))

compile :: Compile PPSL Text
compile v = let _unused = callStack in ppr . fst <$> compile' v

compileSimple :: forall a m. (Applicative m, IsPType (PK m) a) => Term (PK m) a -> m Text
compileSimple v = ppr . fst <$> compile' v

ppr :: PprTerm -> Text
ppr = renderStrict . layoutPretty defaultLayoutOptions {layoutPageWidth = AvailablePerLine 120 1.0} . (`pprTerm` (-10))

type PChurch e = (e #-> e) #-> e #-> e

testTerm :: PPSL edsl => Term edsl (ExampleCase #-> PDiagram ExampleDatum)
testTerm = plam \x -> exampleCases x

compiled :: Text
compiled = runIdentity $ compileSimple testTerm

-- >>> compiled

type Prec = Int

withPrec :: Prec -> Doc a -> Prec -> Doc a
withPrec myPrec raw yourPrec =
  if myPrec < yourPrec
    then parens raw
    else raw

pprBind :: PprBind -> Doc a
pprBind (PprBind lvl _) = pretty $ show lvl

pprType :: PprType -> Prec -> Doc a
pprType = \case
  PprName head args ->
    let call = sep $ pretty head : fmap (`pprType` 11) args
     in if null args then const call else withPrec 10 call
  PprProduct lhs rhs -> withPrec 7 $ pprType lhs 8 <+> "*" <+> pprType rhs 7
  PprSum lhs rhs -> withPrec 6 $ pprType lhs 7 <+> "+" <+> pprType rhs 6
  PprFunc lhs rhs -> withPrec 0 $ pprType lhs 1 <+> "->" <+> pprType rhs 0
  PprList ty -> const $ brackets (pprType ty (-1))

pprTerm :: PprTerm -> Prec -> Doc a
pprTerm = \case
  PprCall head args ->
    let call = nest 2 $ sep $ pretty head : fmap (`pprTerm` 11) args
     in if null args then const call else withPrec 10 call
  PprMonCat lhs rhs -> withPrec 6 $ pprTerm lhs 7 <> softline <> "<>" <+> pprTerm rhs 6
  PprCons hd tl -> withPrec 5 $ pprTerm hd 6 <> softline <> "::" <+> pprTerm tl 5
  PprLam param ty body -> withPrec (-1) $ "\\" <> pprBind param <> ":" <+> pprType ty 0 <+> "->" <> softline <> pprTerm body (-1)
  PprApp f x -> withPrec 10 $ nest 2 $ pprTerm f 10 <+> pprTerm x 11
  PprMatch scrut var branches ->
    withPrec 9 $
      nest 2 $
        "match" <+> pprTerm scrut 0 <+> "as" <+> pprBind var <> line
          <> ( encloseSep -- match exp as ident
                "{ "
                (flatAlt (line <> "}") " }")
                "; " -- { case A -> ...; case B -> ...; case C -> ... }
                (map (\(con, body) -> "case" <+> pretty con <+> "->" <> softline <> pprTerm body 1) branches)
             )
  PprProj obj field -> withPrec 11 $ pprTerm obj 11 <> "." <> pretty field
  PprVar var -> const $ pprBind var
  PprTodo sth -> const $ "<" <> pretty sth <> ">"

instance Monad m => PPSL (PK m) where
  requireInput (MkTerm x) = term do
    x' <- x
    pure $ PprCall "requireInput" [x']
  requireOwnInput (MkTerm x) = term do
    x' <- x
    pure $ PprCall "requireOwnInput" [x']
  createOwnOutput (MkTerm x) = term do
    x' <- x
    pure $ PprCall "createOwnOutput" [x']
  witnessOutput (MkTerm x) = term do
    x' <- x
    pure $ PprCall "witnessOutput" [x']
  createOutput (MkTerm x) = term do
    x' <- x
    pure $ PprCall "createOutput" [x']
  mintOwn (MkTerm x) (MkTerm y) = term do
    x' <- x
    y' <- y
    pure $ PprCall "mintOwn" [x', y']
  witnessMint (MkTerm x) (MkTerm y) (MkTerm z) = term do
    x' <- x
    y' <- y
    z' <- z
    pure $ PprCall "witnessMint" [x', y', z']
  requireSignature (MkTerm x) = term do
    x' <- x
    pure $ PprCall "requireSignature" [x']
  requireValidRange (MkTerm x) = term do
    x' <- x
    pure $ PprCall "requireValidRange" [x']
  requireDCert (MkTerm x) = term do
    x' <- x
    pure $ PprCall "requireDCert" [x']
  toProtocol _ (MkTerm x) (MkTerm y) = term do
    x' <- x
    y' <- y
    pure $ PprCall "toProtocol" [x', y']
  toAddress (MkTerm x) (MkTerm y) (MkTerm z) = term do
    x' <- x
    y' <- y
    z' <- z
    pure $ PprCall "toAddress" [x', y', z']
  fromPkh (MkTerm x) = term do
    x' <- x
    pure $ PprCall "fromPkh" [x']
  utxoRefIs (MkTerm x) (MkTerm y) = term do
    x' <- x
    y' <- y
    pure $ PprCall "utxoRefIs" [x', y']
  emptyValue = mempty
  mkValue (MkTerm x) (MkTerm y) (MkTerm z) = term do
    x' <- x
    y' <- y
    z' <- z
    pure $ PprCall "mkValue" [x', y', z']
  mkAda (MkTerm x) = term do
    x' <- x
    pure $ PprCall "mkAda" [x']
  mkOwnValue (MkTerm x) = term do
    x' <- x
    pure $ PprCall "mkOwnValue" [x']
