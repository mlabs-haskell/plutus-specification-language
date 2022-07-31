{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.PSL.Prettyprint where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
  ( ReaderT (ReaderT, runReaderT),
    ask,
    local,
  )
import Data.Functor.Const
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (callStack)
import qualified Generics.SOP as SOP
import qualified Generics.SOP.GGP as SOP
import qualified Generics.SOP.NP as SOP
import qualified Generics.SOP.NS as SOP
import Plutarch.Core
import Plutarch.EType
import Plutarch.PSL
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import TicTacToe (GameProtocol)

newtype Lvl = Lvl {unLvl :: Int}
  deriving (Num, Show) via Int

data PprType
  = PprName Text [PprType]
  | PprProduct PprType PprType
  | PprSum PprType PprType
  | PprFunc PprType PprType
  deriving stock (Show)

data PprTerm
  = PprCall Text [PprTerm] -- func a b c ...
  | PprMonCat PprTerm PprTerm -- x <> y
  | PprLam PprBind PprType PprTerm -- \x : A. M
  | PprApp PprTerm PprTerm -- f x
  | PprMatch PprTerm PprBind [(Text, PprTerm)] -- match M as x for { ConA -> ...; ConB -> ... }
  | PprProj PprTerm Text -- x.field
  | PprVar PprBind -- x
  | PprTodo Text
  deriving stock (Show)

data PprBind = PprBind Lvl Text -- unique bind\
  deriving stock (Show)

data EDummy (ef :: ETypeF)

instance EIsNewtype EDummy where type EIsNewtype' _ = False

type PprM m = ReaderT (Lvl, Text) m

newtype Ppr m ty = Ppr {runPpr :: PprM m PprTerm}

class TypeReprInfo (m :: Type -> Type) (ty :: EType) where
  typeReprInfo :: Proxy m -> Proxy ty -> PprType

class TypeReprInfo m (EReprAp rep) => TypeInfo' m rep

instance TypeReprInfo m (EReprAp rep) => TypeInfo' m rep

class TypeInfo' m (ERepr ty) => TypeInfo m ty

instance TypeInfo' m (ERepr ty) => TypeInfo m ty

typeInfo :: forall m ty. TypeInfo' m (ERepr ty) => Proxy m -> Proxy ty -> PprType
typeInfo _ _ = typeReprInfo (Proxy @m) (Proxy @(EReprAp (ERepr ty)))

instance EDSL (Ppr m) where
  type IsEType' (Ppr m) = TypeInfo' m

instance TypeReprInfo m EUnit where
  typeReprInfo _ _ = PprName "()" []

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (EPair a b) where
  typeReprInfo _ _ = PprProduct (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @a))

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (EEither a b) where
  typeReprInfo _ _ = PprSum (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @b))

instance (TypeInfo m a, TypeInfo m b) => TypeReprInfo m (a #-> b) where
  typeReprInfo _ _ = PprFunc (typeInfo (Proxy @m) (Proxy @a)) (typeInfo (Proxy @m) (Proxy @b))

instance EIsSOP (Ppr m) a => TypeReprInfo m (ENewtype a) where
  typeReprInfo _ _ = PprName (T.pack $ SOP.datatypeName $ SOP.gdatatypeInfo (Proxy @(EConcrete (Ppr m) a))) []

-- getConst $
--   SOP.cpara_SList @_ @(SOP.All (IsEType (Ppr m))) @inner Proxy (Const []) go
-- where
--   go ::
--     forall y ys.
--     SOP.All (IsEType (Ppr m)) y =>
--     Const [[PprType]] ys ->
--     Const [[PprType]] (y : ys)
--   go (Const ps) = Const (x : ps)
--     where
--       x = getConst $ SOP.cpara_SList @_ @(IsEType (Ppr m)) @y Proxy (Const []) go'
--   go' ::
--     forall y ys.
--     IsEType (Ppr m) y =>
--     Const [PprType] ys ->
--     Const [PprType] (y : ys)
--   go' (Const ps) = Const $ typeInfo (Proxy @m) (Proxy @y) : ps

-- instance TypeReprInfo m EDummy where
--   typeReprInfo _ _ = PprDummy

-- instance (forall e. TypeInfo m e => TypeInfo m (f e)) => TypeReprInfo m (EFix f) where
--   typeReprInfo _ _ = typeInfo (Proxy @m) (Proxy @(f EDummy))

instance TypeInfo m d => TypeReprInfo m (EDiagram d) where
  typeReprInfo _ _ = PprName "Diagram" [typeInfo (Proxy @m) (Proxy @d)]

instance TypeReprInfo m EInteger where typeReprInfo _ _ = PprName "Integer" []

instance TypeReprInfo m EValue where typeReprInfo _ _ = PprName "Value" []

instance TypeReprInfo m EUTXO where typeReprInfo _ _ = PprName "UTXO" []

instance TypeReprInfo m EUTXORef where typeReprInfo _ _ = PprName "UTXORef" []

instance TypeReprInfo m ETokenName where typeReprInfo _ _ = PprName "TokenName" []

instance TypeReprInfo m ECurrencySymbol where typeReprInfo _ _ = PprName "CurrencySymbol" []

instance TypeReprInfo m ETimeRange where typeReprInfo _ _ = PprName "TimeRange" []

instance TypeReprInfo m EPubKeyHash where typeReprInfo _ _ = PprName "PubKeyHash" []

instance TypeReprInfo m EAddress where typeReprInfo _ _ = PprName "Address" []

instance TypeReprInfo m EDCert where typeReprInfo _ _ = PprName "DCert" []

instance TypeReprInfo m EData where typeReprInfo _ _ = PprName "Data" []

getCurrentVar :: Monad m => PprM m PprBind
getCurrentVar = do
  (lvl, nm) <- ask
  pure $ PprBind lvl nm

bound :: Text -> PprM m a -> PprM m a
bound nm m = local (\(l, _) -> (l + 1, nm)) m

instance Applicative m => EConstructable' (Ppr m) (MkETypeRepr EUnit) where
  econImpl _ = Ppr $ pure $ PprCall "Unit" []
  ematchImpl _ f = f EUnit

instance
  (Monad m, IsEType (Ppr m) a, IsEType (Ppr m) b) =>
  EConstructable' (Ppr m) (MkETypeRepr (EPair a b))
  where
  econImpl (EPair (MkTerm x) (MkTerm y)) = Ppr do
    x' <- x
    y' <- y
    pure $ PprCall "Pair" [x', y']
  ematchImpl (Ppr pr) f = term do
    pr' <- pr
    bound "pair" do
      var <- getCurrentVar
      matched <- runTerm $ f $ EPair (pterm $ PprProj (PprVar var) "1") (pterm $ PprProj (PprVar var) "2")
      pure $ PprMatch pr' var [("Pair", matched)]

{-# COMPLETE MkTerm #-}

pattern MkTerm :: forall m a. IsEType' (Ppr m) (ERepr a) => PprM m PprTerm -> Term (Ppr m) a
pattern MkTerm {runTerm} <- (Term (Ppr runTerm))

term :: (IsEType' (Ppr m) (ERepr a) => PprM m PprTerm) -> Term (Ppr m) a
term x = Term (Ppr x)

pterm :: Applicative m => (IsEType' (Ppr m) (ERepr a) => PprTerm) -> Term (Ppr m) a
pterm t = term $ pure t

instance (Monad m, IsEType (Ppr m) a, IsEType (Ppr m) b) => EConstructable' (Ppr m) (MkETypeRepr (EEither a b)) where
  econImpl (ELeft (MkTerm x)) = Ppr $ PprCall "Left" . (: []) <$> x
  econImpl (ERight (MkTerm x)) = Ppr $ PprCall "Right" . (: []) <$> x
  ematchImpl (Ppr et) f = term do
    et' <- et
    bound "either" do
      var <- getCurrentVar
      matchLeft <- runTerm $ f $ ELeft (pterm $ PprVar var)
      matchRight <- runTerm $ f $ ERight (pterm $ PprVar var)
      pure $ PprMatch et' var [("Left", matchLeft), ("Right", matchRight)]

instance (Monad m, IsEType (Ppr m) a, IsEType (Ppr m) b) => EConstructable' (Ppr m) (MkETypeRepr (a #-> b)) where
  econImpl (ELam f) = Ppr $ bound "x" do
    var <- getCurrentVar
    body <- runTerm $ f $ pterm $ PprVar var
    pure $ PprLam var (typeInfo (Proxy @m) (Proxy @a)) body
  ematchImpl (Ppr lm) f = f $ ELam \(MkTerm x) -> term $ PprApp <$> lm <*> x

-- instance (forall a. EConstructable Ppr a => EConstructable Ppr (f a))
--   => EConstructable' (Ppr m) (MkETypeRepr (EFix f)) where

instance Applicative m => EConstructable' (Ppr m) (MkETypeRepr EData) where
  econImpl _ = Ppr $ pure $ PprTodo "BuiltinData"
  ematchImpl _ _ = pterm $ PprTodo "matching on BuiltinData"

-- TODO

instance (Monad m, EIsSOP (Ppr m) a) => EConstructable' (Ppr m) (MkETypeRepr (ENewtype a)) where
  econImpl (ENewtype (x :: a f)) = case esop (Proxy @(Ppr m)) (Proxy @a) of
    EIsSumR _ _ from -> Ppr $ do
      let i = from $ SOP.gfrom x
      let conName = T.pack $ gsFindConName (SOP.constructorInfo $ SOP.gdatatypeInfo $ Proxy @(a f)) i
      xs <-
        sequence $
          SOP.collapse_SOP $
            SOP.cmap_SOP (Proxy @(IsEType (Ppr m))) (SOP.K . runTerm) i
      pure $ PprCall conName xs
  ematchImpl (Ppr x) (f :: ENewtype a f -> Term (Ppr m) b) = case esop (Proxy @(Ppr m)) (Proxy @a) of
    EIsSumR (_ :: Proxy inner) to _ -> term $ bound "c" do
      x' <- x
      var <- getCurrentVar
      let conNames = SOP.collapse_NP $ SOP.map_NP (SOP.K . T.pack . SOP.constructorName) $ SOP.constructorInfo $ SOP.gdatatypeInfo (Proxy @(a f))
      xs <-
        sequence $
          fmap (\x -> runTerm (f $ ENewtype $ SOP.gto $ to x)) $
            SOP.apInjs_POP $ gpAllConProjs (SOP.constructorInfo $ SOP.gdatatypeInfo $ Proxy @(a f)) (PprVar var)
      pure $ PprMatch x' var (zip conNames xs)

gsFindConName :: forall xss' f xss. SOP.NP SOP.ConstructorInfo xss' -> SOP.SOP f xss -> String
gsFindConName (_ SOP.:* cons) (SOP.SOP (SOP.S next)) = gsFindConName cons (SOP.SOP next)
gsFindConName (con SOP.:* _) (SOP.SOP (SOP.Z _)) = SOP.constructorName con
gsFindConName _ _ = undefined

data ConstEx c xss = forall xss'. ConstEx (c xss')

gpAllConProjs ::
  forall xss' m xss.
  (Applicative m, SOP.SListI2 xss', SOP.All2 (IsEType (Ppr m)) xss) =>
  SOP.NP SOP.ConstructorInfo xss' ->
  PprTerm ->
  SOP.POP (Term (Ppr m)) xss
gpAllConProjs info var =
  SOP.POP $
    SOP.cana_NP
      (Proxy @(SOP.All (IsEType (Ppr m))))
      ( \(ConstEx fields) -> case fields of
          SOP.Nil -> undefined
          f SOP.:* fs -> (gpAllProjs f var, ConstEx fs)
      )
      (ConstEx $ SOP.cmap_NP (Proxy @SOP.SListI) getFieldNames info)

getFieldNames :: SOP.SListI xs => SOP.ConstructorInfo xs -> SOP.NP (Const Text) xs
getFieldNames = \case
  SOP.Record _ fs -> SOP.map_NP (\(SOP.FieldInfo nm) -> Const $ T.pack nm) fs
  _ -> SOP.ana_NP (\(Const n) -> (Const $ T.pack $ show n, Const (n + 1))) (Const 1)

gpAllProjs ::
  forall xs' m xs.
  (Applicative m, SOP.All (IsEType (Ppr m)) xs) =>
  SOP.NP (Const Text) xs' ->
  PprTerm ->
  SOP.NP (Term (Ppr m)) xs
gpAllProjs fields var =
  SOP.cana_NP
    (Proxy @(IsEType (Ppr m)))
    ( \(ConstEx fields') -> case fields' of
        SOP.Nil -> undefined
        Const f SOP.:* fs -> (pterm $ PprProj var f, ConstEx fs)
    )
    (ConstEx fields)

instance (Applicative m) => Semigroup (Term (Ppr m) (EDiagram d)) where
  l <> r = term $ PprMonCat <$> runTerm l <*> runTerm r

instance (Applicative m) => Monoid (Term (Ppr m) (EDiagram d)) where
  mempty = pterm $ PprCall "mempty" []

instance (Applicative m) => Semigroup (Term (Ppr m) EValue) where
  (MkTerm l) <> (MkTerm r) = term $ PprMonCat <$> l <*> r

instance (Applicative m) => Monoid (Term (Ppr m) EValue) where
  mempty = pterm $ PprCall "mempty" []

instance Applicative m => EAp m (Ppr m) where
  eapr pre x = term $ (ReaderT $ const pre) *> runTerm x
  eapl x aft = term $ runTerm x <* (ReaderT $ const aft)

instance Monad m => EEmbeds m (Ppr m) where
  eembed m = term do
    tm <- lift m
    runTerm tm

instance Applicative m => EPartial (Ppr m) where
  eerror = pterm $ PprCall "fail" []

compile' :: forall a m. (Applicative m, IsEType (Ppr m) a) => Term (Ppr m) a -> m (PprTerm, PprType)
compile' v = (,) <$> runReaderT (runTerm v) (-1, "") <*> pure (typeInfo (Proxy @m) (Proxy @a))

compile :: Compile EPSL Text
compile v = let _unused = callStack in ppr . fst <$> compile' v

compileSimple :: forall a m. (Applicative m, IsEType (Ppr m) a) => Term (Ppr m) a -> m Text
compileSimple v = ppr . fst <$> compile' v

ppr :: PprTerm -> Text
ppr = renderStrict . layoutPretty defaultLayoutOptions {layoutPageWidth = AvailablePerLine 120 1.0} . (`pprTerm` (-10))

type EChurch e = (e #-> e) #-> e #-> e

testTerm :: EPSL edsl => Term edsl (EChurch EInteger #-> EChurch EInteger #-> EChurch EInteger)
testTerm = elam \x -> elam \y -> elam \s -> elam \z -> x # s # (y # s # z)

compiled :: Text
compiled = runIdentity $ compileSimple testTerm

-- >>> compiled
-- "\\0: (Integer -> Integer) -> Integer -> Integer -> \\1: (Integer -> Integer) -> Integer -> Integer ->\n\\2: Integer -> Integer -> \\3: Integer -> 0 2 (1 2 3)"

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

pprTerm :: PprTerm -> Prec -> Doc a
pprTerm = \case
  PprCall head args ->
    let call = nest 2 $ sep $ pretty head : fmap (`pprTerm` 11) args
     in if null args then const call else withPrec 10 call
  PprMonCat lhs rhs -> withPrec 5 $ pprTerm lhs 6 <> softline <> "<>" <+> pprTerm rhs 5
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

instance Monad m => EPSL (Ppr m) where
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
