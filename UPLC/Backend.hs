{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module UPLC.Backend (CoreTerm, UPLC, compile, compileShow, docToText, evalTerm) where

import Control.Monad.Trans.State (
  StateT,
  evalStateT,
  state,
 )
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP
import Generics.SOP.NP (
  collapse_NP,
  ctraverse'_NP,
  map_NP,
  pure_NP,
 )
import Generics.SOP.NS (
  collapse_SOP,
  index_SOP,
  injections,
  map_SOP,
 )
import PSL (PByteString, PInteger, PNat)
import Plutarch.Core
import Plutarch.Frontends.Data
import Plutarch.PType
import Plutarch.Prelude (
  Generic,
  PHasRepr,
  plam,
 )
import Plutarch.Repr.SOP
import PlutusCore.Default qualified as Core
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as CekCosts
import Prettyprinter (
  Doc,
  Pretty (pretty),
 )
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P
import Universe qualified as Uni
import UntypedPlutusCore qualified as Core
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

type UPLCM (m :: Type -> Type) = StateT Int m

runUPLCM :: Monad m => UPLCM m a -> m a
runUPLCM m = evalStateT m 0

type CoreTerm = Core.Term Core.Name Core.DefaultUni Core.DefaultFun ()

newtype UPLC' m ty = UPLC (Tm m)

type UPLC m = 'PDSLKind (UPLC' m)

type Tm m = UPLCM m CoreTerm

instance PDSL (UPLC m) where
  data IsPTypePrimData (UPLC m) _ = PTy
  newtype PEffect (UPLC m) a = PEff a
    deriving (Functor, Applicative, Monad) via Identity

unPure :: PEffect (UPLC m) a -> a
unPure (PEff x) = x

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall m a. Tm m -> Term (UPLC m) a
pattern MkTerm {runTerm} <- (Term (UPLC runTerm))

term :: Tm m -> Term (UPLC m) a
term x = Term (UPLC x)

pterm :: Monad m => CoreTerm -> Term (UPLC m) a
pterm t = term $ pure t

cconst :: Uni.Contains Core.DefaultUni a => a -> CoreTerm
cconst x = Core.Constant () $ Uni.someValue x

cconstOf :: Core.DefaultUni (Uni.Esc a) -> a -> CoreTerm
cconstOf ty x = Core.Constant () $ Uni.someValueOf ty x

($$) :: CoreTerm -> CoreTerm -> CoreTerm
($$) = Core.Apply ()
infixl 9 $$

cdelay :: CoreTerm -> CoreTerm
cdelay = Core.Delay ()

cforce :: CoreTerm -> CoreTerm
cforce = Core.Force ()

cbuiltin :: Core.DefaultFun -> CoreTerm
cbuiltin = Core.Builtin ()

clam :: Monad m => (CoreTerm -> Tm m) -> Tm m
clam f = do
  x <- state \x -> (x, x + 1)
  let name = Core.Name (T.pack $ show x) (Core.Unique x)
  Core.LamAbs () name <$> f (Core.Var () name)

instance IsPTypePrim (UPLC m) a where
  isPTypePrim = PTy

cunit :: Monad m => Tm m
cunit = clam pure

instance Monad m => PConstructablePrim (UPLC m) PUnit where
  pconImpl _ = UPLC cunit
  pmatchImpl _ f = f PUnit
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

cpair :: Monad m => Tm m -> Tm m -> Tm m
cpair l r = clam \f -> do
  l <- l
  r <- r
  pure $ f $$ l $$ r

cfst :: Monad m => CoreTerm -> Tm m
cfst prod = (prod $$) <$> clam \l -> clam \_r -> pure l

csnd :: Monad m => CoreTerm -> Tm m
csnd prod = (prod $$) <$> clam \_l -> clam pure

instance Monad m => PConstructablePrim (UPLC m) (PPair a b) where
  pconImpl (PPair (MkTerm x) (MkTerm y)) = UPLC $ cpair x y
  pmatchImpl (UPLC prod) f = term do
    prod <- prod
    runTerm $ f $ PPair (term $ cfst prod) (term $ csnd prod)
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

cleft :: Monad m => Tm m -> Tm m
cleft x = clam \l -> clam \_r -> do
  x <- x
  pure $ l $$ x

cright :: Monad m => Tm m -> Tm m
cright x = clam \_l -> clam \r -> do
  x <- x
  pure $ r $$ x

ceither :: Monad m => CoreTerm -> (CoreTerm -> Tm m) -> (CoreTerm -> Tm m) -> Tm m
ceither x l r = do
  lclause <- clam l
  rclause <- clam r
  pure $ x $$ lclause $$ rclause

instance Monad m => PConstructablePrim (UPLC m) (PEither a b) where
  pconImpl (PLeft (MkTerm x)) = UPLC (cleft x)
  pconImpl (PRight (MkTerm x)) = UPLC (cright x)
  pmatchImpl (UPLC sum) f = term do
    sum <- sum
    ceither sum (runTerm . f . PLeft . pterm) (runTerm . f . PLeft . pterm)
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

cabsurd :: Monad m => CoreTerm -> Tm m
cabsurd = pure

instance Monad m => PConstructablePrim (UPLC m) PVoid where
  pconImpl = \case {}
  pmatchImpl (UPLC bot) _ = term $ cabsurd =<< bot
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance Monad m => PConstructablePrim (UPLC m) (a #-> b) where
  pconImpl (PLam f) = UPLC $ clam (runTerm . f . pterm)
  pmatchImpl (UPLC lam) f = f $ PLam \(MkTerm x) -> term do
    lam <- lam
    x <- x
    pure (lam $$ x)
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance Monad m => PConstructablePrim (UPLC m) (PForall1 f) where
  pconImpl (PForall1 (MkTerm x)) = UPLC $ cdelay <$> x
  pmatchImpl (UPLC all) f = f $ PForall1 (term $ cforce <$> all)
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance
  (PGeneric a, Monad m) =>
  PConstructablePrim (UPLC m) (PSOPed a)
  where
  pconImpl (PSOPed x) = UPLC do
    let gx =
          map_SOP (\(Pf' x) -> SOP.K $ runTerm x) $
            pgfrom (Proxy @a) (Proxy @(PConcreteEf (UPLC m))) $
              SOP.gfrom x
    let components = collapse_SOP gx
    let ix = index_SOP gx
    let nCtors = length $ collapse_NP $ pure_NP @_ @(PCode a) (SOP.K ())
    let
      mkVal n f
        | n == nCtors = foldl' ($$) f <$> sequence components
        | n == ix = clam (mkVal (n + 1))
        | otherwise = clam \_ -> mkVal (n + 1) f
    mkVal 0 (Core.Error ())
  pmatchImpl (UPLC x) f = term do
    x <- x
    kases <-
      ctraverse'_NP
        (Proxy @SOP.SListI)
        ( \(inj :: SOP.Injection f xs x) -> do
            let nComponents = length $ collapse_NP $ pure_NP @_ @x (SOP.K ())
            let
              mkCtor 0 xs =
                runTerm
                  . f
                  . PSOPed
                  . SOP.gto
                  . pgto (Proxy @a) (Proxy @(PConcreteEf (UPLC m)))
                  . SOP.SOP
                  . SOP.unK
                  . SOP.apFn inj
                  . map_NP (\(SOP.K term) -> Pf' $ pterm term)
                  . fromJust
                  . SOP.fromList
                  $ reverse xs
              mkCtor n xs = clam \arg -> mkCtor (n - 1) (arg : xs)
            SOP.K <$> mkCtor nComponents []
        )
        (injections @(PCode a))
    pure $ foldl' ($$) x (collapse_NP kases)
  pcaseImpl p f = pure $ pmatchImpl p (unPure . f)

instance Monad m => Num (Term (UPLC m) PInteger) where
  fromInteger n = pterm $ cconst n
  MkTerm m + MkTerm n = term do
    m <- m
    n <- n
    pure $ cbuiltin Core.AddInteger $$ m $$ n
  MkTerm m * MkTerm n = term do
    m <- m
    n <- n
    pure $ cbuiltin Core.MultiplyInteger $$ m $$ n
  MkTerm m - MkTerm n = term do
    m <- m
    n <- n
    pure $ cbuiltin Core.SubtractInteger $$ m $$ n
  abs (MkTerm m) = term do
    m <- m
    pure $
      cforce (cbuiltin Core.IfThenElse)
        $$ (cbuiltin Core.EqualsInteger $$ m $$ cconst @Integer 0)
        $$ cconst @Integer 0
        $$ ( cforce (cbuiltin Core.IfThenElse)
              $$ (cbuiltin Core.LessThanInteger $$ m $$ cconst @Integer 0)
              $$ cconst @Integer (-1)
              $$ cconst @Integer 1
           )
  signum (MkTerm m) = term do
    m <- m
    pure $
      cforce (cbuiltin Core.IfThenElse)
        $$ (cbuiltin Core.LessThanInteger $$ m $$ cconst @Integer 0)
        $$ (cbuiltin Core.SubtractInteger $$ cconst @Integer 0 $$ m)
        $$ m

instance Monad m => IsString (Term (UPLC m) PByteString) where
  fromString xs = pterm $ cconst $ T.encodeUtf8 $ T.pack xs

instance Monad m => Semigroup (Term (UPLC m) PByteString) where
  (MkTerm xs) <> (MkTerm ys) = term do
    xs <- xs
    ys <- ys
    pure $ cbuiltin Core.AppendByteString $$ xs $$ ys

instance Monad m => Monoid (Term (UPLC m) PByteString) where
  mempty = pterm $ cconst @ByteString ""

compile :: forall m a. Monad m => Term (UPLC m) a -> m CoreTerm
compile (MkTerm v) = runUPLCM v

compileShow :: Term (UPLC Identity) a -> Text
compileShow = docToText . pretty . runIdentity . compile

docToText :: Doc a -> Text
docToText = P.renderStrict . P.layoutPretty P.defaultLayoutOptions {P.layoutPageWidth = P.AvailablePerLine 120 1.0}

evalTerm :: CoreTerm -> Either (Cek.CekEvaluationException Core.Name Core.DefaultUni Core.DefaultFun) CoreTerm
evalTerm term =
  let (result, _, _) = Cek.runCek (CekCosts.unitCekParameters @()) Cek.counting Cek.logEmitter term
   in result

testTerm :: Term (UPLC Identity) (PEither PUnit PVoid)
testTerm = pcon (PLeft $ pcon PUnit)

pswap :: Term (UPLC Identity) (PEither a b #-> PEither b a)
pswap = plam \x -> pmatch x \case
  PLeft x -> pcon $ PRight x
  PRight x -> pcon $ PLeft x

data TestData f
  = TestNone
  | TestOne (f /$ PUnit)
  | TestTwo (f /$ PUnit) (f /$ PUnit)
  deriving stock (Generic)
  deriving anyclass (PHasRepr)

testData :: Monad m => Term (UPLC m) TestData
testData = pmatch (pcon $ TestOne (pcon PUnit)) \case
  TestNone -> pcon $ TestOne (pcon PUnit)
  TestOne _ -> pcon $ TestTwo (pcon PUnit) (pcon PUnit)
  TestTwo _ _ -> pcon TestNone

myInteger :: Monad m => Term (UPLC m) PInteger
myInteger = 1 + 2 * 3

myByteString :: Monad m => Term (UPLC m) PByteString
myByteString = "hello " <> mempty <> "world"

-- >>> either (T.pack . show) (docToText . pretty) $ evalTerm $ runIdentity $ compile $ myByteString
-- "(con bytestring #68656c6c6f20776f726c64)"
