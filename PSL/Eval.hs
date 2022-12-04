{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Eval where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Generics.SOP (
  K (K),
  SOP,
  constructorInfo,
  constructorName,
  datatypeName,
  unK,
 )
import Generics.SOP.GGP (gdatatypeInfo, gfrom, gto)
import Generics.SOP.NP (collapse_NP, map_NP)
import Generics.SOP.NS (
  cmap_SOP,
  collapse_SOP,
  ctraverse'_SOP,
  index_SOP,
 )
import Numeric.Natural (Natural)
import PSL
import Plutarch.Core
import Plutarch.Lam
import Plutarch.PType
import Prettyprinter (
  Doc,
  LayoutOptions (layoutPageWidth),
  PageWidth (AvailablePerLine),
  Pretty (pretty),
  brackets,
  defaultLayoutOptions,
  dquotes,
  layoutPretty,
  parens,
  sep,
  (<+>),
 )
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)
import Unsafe.Coerce (unsafeCoerce)

data Val
  = VLam (Val -> Val) -- \x. M
  | VPair Val Val -- (x, y)
  | VEither (Either Val Val) -- Left x / Right y
  | VInt Integer -- 123
  | VBS ByteString -- "abc"
  | VNat Natural
  | VList [Val] -- [a, b, c]
  | VUnit -- ()
  | forall a. PIsSOP EK a => VSOP (Proxy a) (SOP (K Val) (PSOPPTypes EK a))
  | VPubKeyHash PubKeyHash
  | VValidatorHash ValidatorHash
  | VCurrencySymbol CurrencySymbol
  | VTokenName TokenName
  | VAddress Address
  | VValue Value
  | VData Data

data Ty
  = TFun Ty Ty -- A -> B
  | TProd Ty Ty -- (A, B)
  | TSum Ty Ty -- A | B
  | TInt -- Integer
  | TBS -- ByteString
  | TNat -- Natural
  | TList Ty -- [A]
  | TUnit -- ()
  | forall a. PIsSOP EK a => TSOP (Proxy a)
  | TPubKeyHash
  | TValidatorHash
  | TCurrencySymbol
  | TTokenName
  | TAddress
  | TValue
  | TData

intoLam :: Val -> Val -> Val
intoLam = \case
  VLam lam -> lam
  _ -> error "absurd: a function is not a lambda"
intoEither :: Val -> Either Val Val
intoEither = \case
  VEither x -> x
  _ -> error "absurd: a sum is not an either"
intoPair :: Val -> (Val, Val)
intoPair = \case
  VPair l r -> (l, r)
  _ -> error "absurd: a product is not a pair"
intoInt :: Val -> Integer
intoInt = \case
  VInt n -> n
  _ -> error "absurd: an int is not an integer"
intoBS :: Val -> ByteString
intoBS = \case
  VBS bs -> bs
  _ -> error "absurd: a bytestring is not a bytestring"
intoNat :: Val -> Natural
intoNat = \case
  VNat n -> n
  _ -> error "absurd: a nat is not a natural"
intoUnit :: Val -> ()
intoUnit = \case
  VUnit -> ()
  _ -> error "absurd: a unit is not a unit"
intoList :: Val -> [Val]
intoList = \case
  VList xs -> xs
  _ -> error "absurd: a list is not a list"
intoData :: Val -> Data
intoData = \case
  VData d -> d
  _ -> error "absurd: a Data is not a Data"
intoValue :: Val -> Value
intoValue = \case
  VValue val -> val
  _ -> error "absurd: a Value is not a Value"

newtype Value = Value {unValue :: Map CurrencySymbol (Map TokenName Integer)}
  deriving stock (Eq, Ord)
newtype PubKeyHash = PubKeyHash {unPubKeyHash :: ByteString}
  deriving stock (Eq, Ord)
newtype ValidatorHash = ValidatorHash {unValidatorHash :: ByteString}
  deriving stock (Eq, Ord)
newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: ByteString}
  deriving stock (Eq, Ord)
newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving stock (Eq, Ord)
data Address = AddrPubKey PubKeyHash | AddrValidator ValidatorHash
  deriving stock (Eq, Ord)
data Data
  = DataConstr Integer [Data]
  | DataMap [(Data, Data)]
  | DataList [Data]
  | DataInt Integer
  | DataBS ByteString
  deriving stock (Eq, Ord)

toHex :: ByteString -> String
toHex bs = do
  x <- BS.unpack bs
  fmap (intToDigit . fromIntegral) [x `div` 16, x `mod` 16]

instance Pretty PubKeyHash where
  pretty (PubKeyHash x) = parens $ "PubKeyHash" <+> pretty (toHex x)

instance Pretty ValidatorHash where
  pretty (ValidatorHash x) = parens $ "ValidatorHash" <+> pretty (toHex x)

instance Pretty CurrencySymbol where
  pretty (CurrencySymbol x) = parens $ "CurrencySymbol" <+> pretty (toHex x)

instance Pretty TokenName where
  pretty (TokenName x) = parens $ "TokenName" <+> dquotes (pretty $ T.unpack $ decodeUtf8 x)

instance Pretty Address where
  pretty (AddrPubKey pkh) = parens $ "AddrPubKey" <+> parens (pretty pkh)
  pretty (AddrValidator vh) = parens $ "AddrValidator" <+> parens (pretty vh)

instance Semigroup Value where
  Value xs <> Value ys =
    Value $
      Map.filter Map.null $
        Map.map (Map.filter (== 0)) $
          Map.unionWith (Map.unionWith (+)) xs ys

instance Monoid Value where
  mempty = Value Map.empty

instance Pretty Value where
  pretty (Value xss) =
    parens $
      let prettyInner = P.list . fmap (\(tn, x) -> pretty tn <> ":" <+> pretty x) . Map.toList
          prettyOuter = P.list . fmap (\(cs, xs) -> pretty cs <> ":" <+> prettyInner xs) . Map.toList
       in "Value" <+> prettyOuter xss

instance Pretty Data where
  pretty =
    parens . \case
      DataConstr n x -> "DataConstr" <+> pretty n <+> pretty x
      DataMap xs -> "DataMap" <+> P.list (fmap (\(k, v) -> pretty k <> ":" <+> pretty v) xs)
      DataList xs -> "DataList" <+> pretty xs
      DataInt n -> "DataInt" <+> pretty n
      DataBS bs -> "DataBS" <+> dquotes (pretty $ decodeUtf8 bs)

type EvalM = Identity

newtype Eval ty = Eval {runEval :: EvalM Val}

type EK = 'PDSLKind Eval

type TypeReprInfo :: forall (a :: PType). PHs a -> Constraint
class TypeReprInfo ty where
  typeReprInfo :: Proxy ty -> Ty

instance PDSL EK where
  type IsPTypeBackend EK = TypeReprInfo

class (IsPType EK a) => TypeInfo a
instance (IsPType EK a) => TypeInfo a

typeInfo :: forall a. TypeInfo a => Proxy a -> Ty
typeInfo _ = isPType (Proxy @EK) (Proxy @a) typeReprInfo

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (PEither a b) where
  typeReprInfo _ = TSum (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (PPair a b) where
  typeReprInfo _ = TProd (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance (TypeInfo a, TypeInfo b) => TypeReprInfo (a #-> b) where
  typeReprInfo _ = TFun (typeInfo (Proxy @a)) (typeInfo (Proxy @b))

instance TypeInfo a => TypeReprInfo (PList a) where
  typeReprInfo _ = TList (typeInfo (Proxy @a))

instance PIsSOP EK a => TypeReprInfo (PSOPed a) where
  typeReprInfo _ = TSOP (Proxy @a)

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

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall a. EvalM Val -> Term EK a
pattern MkTerm {runTerm} <- (Term (Eval runTerm))

term :: EvalM Val -> Term EK a
term x = Term (Eval x)

pterm :: Val -> Term EK a
pterm t = term $ pure t

instance PConstructable' EK PUnit where
  pconImpl _ = Eval $ pure VUnit
  pmatchImpl _ f = f PUnit

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructable' EK (PPair a b)
  where
  pconImpl (PPair (MkTerm x) (MkTerm y)) = Eval $ VPair <$> x <*> y
  pmatchImpl (Eval prod) f = term do
    (l, r) <- intoPair <$> prod
    runTerm $ f $ PPair (pterm l) (pterm r)

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructable' EK (PEither a b)
  where
  pconImpl (PLeft (MkTerm x)) = Eval $ VEither . Left <$> x
  pconImpl (PRight (MkTerm x)) = Eval $ VEither . Right <$> x
  pmatchImpl (Eval sum) f = term do
    sum' <- intoEither <$> sum
    case sum' of
      Left l -> runTerm $ f $ PLeft (pterm l)
      Right r -> runTerm $ f $ PRight (pterm r)

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructable' EK (a #-> b)
  where
  pconImpl (PLam f) = Eval $ pure $ VLam $ \x -> runIdentity $ runTerm $ f $ pterm x
  pmatchImpl (Eval lam) f = term do
    lam' <- intoLam <$> lam
    runTerm $ f $ PLam \(MkTerm x) -> term $ lam' <$> x

instance PConstructable' EK PNat where
  pconImpl PZ = Eval $ pure $ VNat 0
  pconImpl (PS (MkTerm n)) = Eval do
    n' <- intoNat <$> n
    pure $ VNat $ n' + 1
  pmatchImpl (Eval n) f = term do
    n' <- intoNat <$> n
    case n' of
      0 -> runTerm $ f PZ
      x -> runTerm $ f $ PS (pterm $ VNat $ x - 1)

instance TypeInfo a => PConstructable' EK (PList a) where
  pconImpl PNil = Eval $ pure $ VList []
  pconImpl (PCons (MkTerm x) (MkTerm xs)) = Eval do
    x' <- x
    xs' <- intoList <$> xs
    pure $ VList $ x' : xs'
  pmatchImpl (Eval xs) f = term do
    xs' <- intoList <$> xs
    case xs' of
      [] -> runTerm $ f PNil
      x : xs'' -> runTerm $ f $ PCons (pterm x) (pterm $ VList xs'')

instance
  PIsSOP EK a =>
  PConstructable' EK (PSOPed a)
  where
  pconImpl (PSOPed x) = Eval do
    gx <-
      ctraverse'_SOP (Proxy @(IsPType EK)) (fmap K . unK) $
        cmap_SOP (Proxy @(IsPType EK)) (K . runTerm) $
          sopFrom (Proxy @EK) (Proxy @a) $
            gfrom x
    pure $ VSOP (Proxy @a) gx
  pmatchImpl (Eval x) f = term do
    x >>= \case
      VSOP _ x' -> do
        let unGx =
              gto $
                sopTo (Proxy @EK) (Proxy @a) $
                  -- it MUST be this particular type, but we can't possibly know
                  unsafeCoerce @(SOP (Term EK) _) @(SOP (Term EK) (PSOPPTypes EK a)) $
                    cmap_SOP (Proxy @(IsPType EK)) (pterm . unK) x'
        runTerm $ f $ PSOPed unGx
      _ -> error "absurd: an SOPed is not an SOP"

instance PConstructable' EK PData where
  pconImpl x = Eval case x of
    PDataConstr (MkTerm var) (MkTerm vals) -> do
      var' <- intoInt <$> var
      vals' <- intoList <$> vals
      pure $ VData $ DataConstr var' (intoData <$> vals')
    PDataMap (MkTerm xs) -> do
      xs' <- intoList <$> xs
      pure $ VData $ DataMap (bimap intoData intoData . intoPair <$> xs')
    PDataList (MkTerm xs) -> do
      xs' <- intoList <$> xs
      pure $ VData $ DataList (intoData <$> xs')
    PDataInteger (MkTerm n) -> do
      ni <- intoInt <$> n
      pure $ VData $ DataInt ni
    PDataByteString (MkTerm bs) -> do
      bs' <- intoBS <$> bs
      pure $ VData $ DataBS bs'
  pmatchImpl (Eval x) f = term do
    d <- intoData <$> x
    case d of
      DataConstr var vals -> runTerm $ f $ PDataConstr (pterm $ VInt var) (pterm $ VList $ fmap VData vals)
      DataMap xs -> runTerm $ f $ PDataMap (pterm $ VList $ fmap (uncurry VPair . bimap VData VData) xs)
      DataList xs -> runTerm $ f $ PDataList (pterm $ VList $ fmap VData xs)
      DataInt n -> runTerm $ f $ PDataInteger (pterm $ VInt n)
      DataBS bs -> runTerm $ f $ PDataByteString (pterm $ VBS bs)

instance Num (Term EK PInteger) where
  MkTerm x + MkTerm y = term $ VInt <$> liftA2 (+) (intoInt <$> x) (intoInt <$> y)
  MkTerm x - MkTerm y = term $ VInt <$> liftA2 (-) (intoInt <$> x) (intoInt <$> y)
  MkTerm x * MkTerm y = term $ VInt <$> liftA2 (*) (intoInt <$> x) (intoInt <$> y)
  abs (MkTerm x) = term $ VInt . abs . intoInt <$> x
  signum (MkTerm x) = term $ VInt . signum . intoInt <$> x
  fromInteger n = pterm $ VInt n

instance IsString (Term EK PByteString) where
  fromString xs = pterm $ VBS $ encodeUtf8 $ T.pack xs

instance Semigroup (Term EK PByteString) where
  MkTerm x <> MkTerm y = term $ VBS <$> liftA2 (<>) (intoBS <$> x) (intoBS <$> y)

instance Monoid (Term EK PByteString) where
  mempty = pterm $ VBS BS.empty

instance Semigroup (Term EK PValue) where
  MkTerm x <> MkTerm y = term $ VValue <$> liftA2 (<>) (intoValue <$> x) (intoValue <$> y)

instance Monoid (Term EK PValue) where
  mempty = pterm $ VValue mempty

compile :: forall a. IsPType EK a => Term EK a -> (Val, Ty)
compile v =
  let val = runIdentity $ runTerm v
      ty = typeInfo (Proxy @a)
   in (val, ty)

instance Pretty Ty where
  pretty = \case
    TFun a b -> parens (pretty a <+> "->" <+> pretty b)
    TProd a b -> parens (pretty a <> "," <+> pretty b)
    TSum a b -> parens (pretty a <+> "|" <+> pretty b)
    TInt -> "Integer"
    TBS -> "ByteString"
    TNat -> "Natural"
    TUnit -> "()"
    TList a -> brackets (pretty a)
    TSOP (_ :: Proxy a) -> pretty $ datatypeName $ gdatatypeInfo (Proxy @(PConcrete EK a))
    TPubKeyHash -> "PubKeyHash"
    TValidatorHash -> "ValidatorHash"
    TCurrencySymbol -> "CurrencySymbol"
    TTokenName -> "TokenName"
    TAddress -> "Address"
    TValue -> "Value"
    TData -> "Data"

instance Pretty Val where
  pretty = \case
    VLam _ -> "[lambda]"
    VPair x y -> parens (pretty x <> "," <+> pretty y)
    VEither et -> case et of
      Left x -> parens ("Left" <+> pretty x)
      Right y -> parens ("Right" <+> pretty y)
    VInt n -> pretty n
    VBS bs -> dquotes $ pretty $ decodeUtf8 bs
    VUnit -> "()"
    VNat n -> pretty n
    VList xs -> P.list $ fmap pretty xs
    VSOP (_ :: Proxy a) x ->
      let conName =
            collapse_NP
              ( map_NP (K . constructorName) $
                  constructorInfo $
                    gdatatypeInfo $
                      Proxy @(PConcrete EK a)
              )
              !! index_SOP x
          xs = pretty <$> collapse_SOP x
       in if null xs
            then pretty conName
            else parens (pretty conName <+> sep xs)
    VPubKeyHash pkh -> pretty pkh
    VValidatorHash vh -> pretty vh
    VCurrencySymbol cs -> pretty cs
    VTokenName nm -> pretty nm
    VAddress addr -> pretty addr
    VValue val -> pretty val
    VData dt -> pretty dt

docToText :: Doc a -> Text
docToText = renderStrict . layoutPretty defaultLayoutOptions {layoutPageWidth = AvailablePerLine 120 1.0}

compileShow :: IsPType EK a => Term EK a -> Text
compileShow = docToText . pretty . fst . compile

intTerm :: Term EK PInteger
intTerm = (plam \x -> plam \y -> plam \z -> x * y + z) # 3 # 2 # 1

-- >>> compileShow intTerm
-- "7"

boolTerm :: Term EK PBool
boolTerm = pcon PTrue

-- >>> compileShow boolTerm
-- "PTrue"

bsTerm :: Term EK PByteString
bsTerm = "hello " <> "world"

-- >>> compileShow bsTerm
-- "\"hello world\""

pairTerm :: Term EK (PPair PInteger (PPair PBool PByteString))
pairTerm = pcon (PPair intTerm (pcon $ PPair boolTerm bsTerm))

-- >>> compileShow pairTerm
-- "(7, (PTrue, \"hello world\"))"
