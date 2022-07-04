{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.PSL.Prettyprint where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask, local)
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
  | PprLam Text PprType PprTerm -- \x : A. M
  | PprApp PprTerm PprTerm -- f x
  | PprMatch PprTerm Text [(Text, PprTerm)] -- match M as x for { ConA -> ...; ConB -> ... }
  | PprProj PprTerm Text -- x.field
  | PprVar Lvl Text -- x
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

getCurrentVar :: Monad m => PprM m PprTerm
getCurrentVar = do
  (lvl, nm) <- ask
  pure $ PprVar lvl nm

getCurrentVarName :: Monad m => PprM m Text
getCurrentVarName = do
  (_, nm) <- ask
  pure nm

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
      nm <- getCurrentVarName
      matched <- runTerm $ f $ EPair (pterm $ PprProj var "1") (pterm $ PprProj var "2")
      pure $ PprMatch pr' nm [("Pair", matched)]

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
      nm <- getCurrentVarName
      matchLeft <- runTerm $ f $ ELeft (pterm var)
      matchRight <- runTerm $ f $ ERight (pterm var)
      pure $ PprMatch et' nm [("Left", matchLeft), ("Right", matchRight)]

instance (Monad m, IsEType (Ppr m) a, IsEType (Ppr m) b) => EConstructable' (Ppr m) (MkETypeRepr (a #-> b)) where
  econImpl (ELam f) = Ppr $ bound "x" do
    var <- getCurrentVar
    nm <- getCurrentVarName
    body <- runTerm $ f $ pterm var
    pure $ PprLam nm (typeInfo (Proxy @m) (Proxy @a)) body
  ematchImpl (Ppr lm) f = f $ ELam \(MkTerm x) -> term $ PprApp <$> lm <*> x

-- instance (forall a. EConstructable Ppr a => EConstructable Ppr (f a))
--   => EConstructable' (Ppr m) (MkETypeRepr (EFix f)) where

instance EConstructable' (Ppr m) (MkETypeRepr EData)

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
      nm <- getCurrentVarName
      let conNames = SOP.collapse_NP $ SOP.map_NP (SOP.K . T.pack . SOP.constructorName) $ SOP.constructorInfo $ SOP.gdatatypeInfo (Proxy @(a f))
      xs <-
        sequence $
          fmap (\x -> runTerm (f $ ENewtype $ SOP.gto $ to x)) $
            SOP.apInjs_POP $ gpAllConProjs (SOP.constructorInfo $ SOP.gdatatypeInfo $ Proxy @(a f)) var
      pure $ PprMatch x' nm (zip conNames xs)

gsFindConName :: forall xss' f xss. SOP.NP SOP.ConstructorInfo xss' -> SOP.SOP f xss -> String
gsFindConName (_ SOP.:* cons) (SOP.SOP (SOP.S next)) = gsFindConName cons (SOP.SOP next)
gsFindConName (con SOP.:* _) (SOP.SOP (SOP.Z _)) = SOP.constructorName con
gsFindConName _ _ = undefined

data ConstEx c xss = forall xss'. ConstEx (c xss')

gpAllConProjs :: forall xss' m xss. (Applicative m, SOP.SListI2 xss', SOP.All2 (IsEType (Ppr m)) xss) => SOP.NP SOP.ConstructorInfo xss' -> PprTerm -> SOP.POP (Term (Ppr m)) xss
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

gpAllProjs :: forall xs' m xs. (Applicative m, SOP.All (IsEType (Ppr m)) xs) => SOP.NP (Const Text) xs' -> PprTerm -> SOP.NP (Term (Ppr m)) xs
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

compile' :: forall a m. (Applicative m, IsEType (Ppr m) a) => Term (Ppr m) a -> m (PprTerm, PprType)
compile' v = (,) <$> runReaderT (runTerm v) (-1, "") <*> pure (typeInfo (Proxy @m) (Proxy @a))

compile :: Compile ESOP Text
compile v = let _unused = callStack in ppr . fst <$> compile' v

compileSimple :: forall a m. (Applicative m, IsEType (Ppr m) a) => Term (Ppr m) a -> m Text
compileSimple v = ppr . fst <$> compile' v

ppr :: PprTerm -> Text
ppr = T.pack . show

testTerm :: ESOP edsl => Term edsl (EUnit #-> EUnit)
testTerm = econ (ELam \x -> x)

compiled :: Text
compiled = runIdentity $ compileSimple testTerm

-- >>> compiled
-- "PprLam \"x\" (PprName \"()\" []) (PprVar 0 \"x\")"

-- TODO: proper prettyprinting
-- TODO: EPSL builtins

-- instance Monad m => EPSL (Ppr m) where
--   requireInput (Term x) = term \lvl -> PprBuiltin "requireInput" [runPpr x lvl]