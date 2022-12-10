{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Eval.Backend where

import Control.Applicative (Applicative (liftA2))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 qualified as Time
import Generics.SOP (SOP)
import Generics.SOP qualified as SOP
import Generics.SOP.GGP qualified as SOP
import Generics.SOP.NP (collapse_NP, map_NP)
import Generics.SOP.NS (cmap_SOP, collapse_SOP, ctraverse'_SOP, index_SOP)
import Numeric.Natural (Natural)
import PSL
import PSL.Eval.Interval (Interval)
import PSL.Eval.Interval qualified as Iv
import PSL.Eval.TH (mkConvHelper, mkConvHelper')
import PSL.Eval.Tx
import Plutarch.Core
import Plutarch.PType
import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Backend types ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The context for evaluation. This is currently empty as we don't need any environment value or internal state yet.
type EvalM = Identity

-- | A computation that returns an evaluated value.
newtype Eval ty = Eval {runEval :: EvalM Val}

-- | The NbE backend.
type EK = 'PDSLKind Eval

instance PDSL EK where
  type IsPTypeBackend EK = TypeReprInfo

type TypeReprInfo :: forall (a :: PType). PHs a -> Constraint
class TypeReprInfo ty where
  typeReprInfo :: Proxy ty -> Ty

class (IsPType EK a) => TypeInfo a
instance (IsPType EK a) => TypeInfo a

typeInfo :: forall a. TypeInfo a => Proxy a -> Ty
typeInfo _ = isPType (Proxy @EK) (Proxy @a) typeReprInfo

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall a. EvalM Val -> Term EK a
pattern MkTerm {runTerm} <- (Term (Eval runTerm))

term :: EvalM Val -> Term EK a
term x = Term (Eval x)

pterm :: Val -> Term EK a
pterm t = term $ pure t

-- | The evaluation value. Non-normal forms are excluded.
data Val
  = VLam (Val -> Val) -- \x. M
  | VPair (Val, Val) -- (x, y)
  | VEither (Either Val Val) -- Left x / Right y
  | VInt Integer -- 123
  | VBS ByteString -- "abc"
  | VNat Natural
  | VList [Val] -- [a, b, c]
  | VUnit -- ()
  | forall a. PIsSOP EK a => VSOP (Proxy a) (SOP (SOP.K Val) (PSOPPTypes EK a))
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
  | forall a. PIsSOP EK a => TSOP (Proxy a)
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

--------------------------------------------------------------------------------
-- Transaction types -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Diagram backend's representation of a non-own UTXO.
data UTXO
  = -- | UTXO from an address.
    UTXOAddr Address Value Data
  | -- | UTXO from a script belonging to a certain protocol.
    UTXOProtocol String Value Val

-- | UTXO of the current protocol.
data OwnUTXO = OwnUTXO Value Val

-- | Transaction outputs.
data Output
  = -- | The output must happen, but need not be unique.
    OutputWitness UTXO
  | -- | The output must happen, and must be unique.
    OutputCreate UTXO
  | OutputOwn OwnUTXO

newtype EvalTime = EvalTime POSIXTime
  deriving (Eq, Ord, Enum) via POSIXTime

instance Pretty EvalTime where
  pretty (EvalTime t) = pretty $ Time.iso8601Show $ posixSecondsToUTCTime t

type TimeRange = Interval EvalTime

-- | A transaction diagram.
data Diagram = Diagram
  { diagOwnInputs :: [OwnUTXO]
  -- ^ Input from the current protocol.
  , diagInputs :: Set UTXORef
  -- ^ References to inputs from other protocols.
  , diagMap :: [(UTXORef, UTXO)]
  -- ^ Mapping from UTXO references to actual UTXOs. 1-N relationships are bogus.
  , diagOutputs :: [Output]
  -- ^ Outputs of the transaction.
  , diagSign :: Set PubKeyHash
  -- ^ Signatories of the transaction.
  , diagMint :: Value
  -- ^ Minted value of the transaction.
  , diagTimeRange :: TimeRange
  -- ^ The time range in which the transaction should be valid.
  }

instance Semigroup Diagram where
  Diagram ownIns ins inMap outs sign mint tr <> Diagram ownIns' ins' inMap' outs' sign' mint' tr' =
    Diagram
      (ownIns <> ownIns')
      (ins <> ins')
      (inMap <> inMap')
      (outs <> outs')
      (sign <> sign')
      (mint <> mint')
      (tr `Iv.intersection` tr')

instance Monoid Diagram where
  mempty = Diagram mempty mempty mempty mempty mempty mempty Iv.always

--------------------------------------------------------------------------------
-- Conversion helpers ----------------------------------------------------------
--------------------------------------------------------------------------------

mkConvHelper "Lam" [t|Val -> Val|]
mkConvHelper "Either" [t|Either Val Val|]
mkConvHelper "Pair" [t|(Val, Val)|]
mkConvHelper "Int" [t|Integer|]
mkConvHelper "BS" [t|ByteString|]
mkConvHelper "Nat" [t|Natural|]
mkConvHelper "List" [t|[Val]|]
mkConvHelper' "Data"
mkConvHelper' "Value"
mkConvHelper' "CurrencySymbol"
mkConvHelper' "TokenName"
mkConvHelper' "PubKeyHash"
mkConvHelper' "Address"
mkConvHelper' "UTXO"
mkConvHelper' "OwnUTXO"
mkConvHelper' "Diagram"
mkConvHelper' "UTXORef"
mkConvHelper' "TimeRange"

intoUnit :: Val -> ()
intoUnit = \case
  VUnit -> ()
  _ -> error "absurd: a unit is not a unit"

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

instance PIsSOP EK a => TypeReprInfo (PSOPed a) where
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

instance PConstructable' EK PUnit where
  pconImpl _ = Eval $ pure VUnit
  pmatchImpl _ f = f PUnit

instance
  (TypeInfo a, TypeInfo b) =>
  PConstructable' EK (PPair a b)
  where
  pconImpl (PPair (MkTerm x) (MkTerm y)) = Eval $ VPair <$> ((,) <$> x <*> y)
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
      ctraverse'_SOP (Proxy @(IsPType EK)) (fmap SOP.K . SOP.unK) $
        cmap_SOP (Proxy @(IsPType EK)) (SOP.K . runTerm) $
          sopFrom (Proxy @EK) (Proxy @a) $
            SOP.gfrom x
    pure $ VSOP (Proxy @a) gx
  pmatchImpl (Eval x) f = term do
    x >>= \case
      VSOP _ x' -> do
        let unGx =
              SOP.gto $
                sopTo (Proxy @EK) (Proxy @a) $
                  -- it MUST be this particular type, but we can't possibly know
                  unsafeCoerce @(SOP (Term EK) _) @(SOP (Term EK) (PSOPPTypes EK a)) $
                    cmap_SOP (Proxy @(IsPType EK)) (pterm . SOP.unK) x'
        runTerm $ f $ PSOPed unGx
      _ -> error "absurd: an SOPed is not an SOP"

instance IsPType EK d => PConstructable' EK (POwnUTXO d) where
  pconImpl (POwnUTXO (MkTerm val) (MkTerm dat)) = Eval do
    val' <- intoValue <$> val
    VOwnUTXO . OwnUTXO val' <$> dat
  pmatchImpl (Eval x) f = term do
    OwnUTXO val dat <- intoOwnUTXO <$> x
    runTerm $ f $ POwnUTXO (pterm $ VValue val) (pterm dat)

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
      DataMap xs -> runTerm $ f $ PDataMap (pterm $ VList $ fmap (VPair . bimap VData VData) xs)
      DataList xs -> runTerm $ f $ PDataList (pterm $ VList $ fmap VData xs)
      DataInt n -> runTerm $ f $ PDataInteger (pterm $ VInt n)
      DataBS bs -> runTerm $ f $ PDataByteString (pterm $ VBS bs)

--------------------------------------------------------------------------------
-- Other typeclass support -----------------------------------------------------
--------------------------------------------------------------------------------

instance Num (Term EK PInteger) where
  MkTerm x + MkTerm y = term $ VInt <$> liftA2 (+) (intoInt <$> x) (intoInt <$> y)
  MkTerm x - MkTerm y = term $ VInt <$> liftA2 (-) (intoInt <$> x) (intoInt <$> y)
  MkTerm x * MkTerm y = term $ VInt <$> liftA2 (*) (intoInt <$> x) (intoInt <$> y)
  abs (MkTerm x) = term $ VInt . abs . intoInt <$> x
  signum (MkTerm x) = term $ VInt . signum . intoInt <$> x
  fromInteger n = pterm $ VInt n

instance IsString (Term EK PByteString) where
  fromString xs = pterm $ VBS $ T.encodeUtf8 $ T.pack xs

instance Semigroup (Term EK PByteString) where
  MkTerm x <> MkTerm y = term $ VBS <$> liftA2 (<>) (intoBS <$> x) (intoBS <$> y)

instance Monoid (Term EK PByteString) where
  mempty = pterm $ VBS BS.empty

instance Semigroup (Term EK PValue) where
  MkTerm x <> MkTerm y = term $ VValue <$> liftA2 (<>) (intoValue <$> x) (intoValue <$> y)

instance Monoid (Term EK PValue) where
  mempty = pterm $ VValue mempty

-- TODO: Finish Monoid instance of Diagram
instance Semigroup (Term EK (PDiagram d)) where
  MkTerm x <> MkTerm y = term $ VDiagram <$> liftA2 (<>) (intoDiagram <$> x) (intoDiagram <$> y)

instance Monoid (Term EK (PDiagram d)) where
  mempty = pterm $ VDiagram mempty

--------------------------------------------------------------------------------
-- Speccing support ------------------------------------------------------------
--------------------------------------------------------------------------------

instance PPSL EK where
  requireInput (MkTerm ref) = term do
    ref' <- intoUTXORef <$> ref
    pure $ VDiagram $ mempty {diagInputs = Set.singleton ref'}
  requireOwnInput (MkTerm utxo) = term do
    utxo' <- intoOwnUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOwnInputs = [utxo']}
  createOwnOutput (MkTerm utxo) = term do
    utxo' <- intoOwnUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [OutputOwn utxo']}
  witnessOutput (MkTerm utxo) = term do
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [OutputWitness utxo']}
  createOutput (MkTerm utxo) = term do
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagOutputs = [OutputCreate utxo']}
  mintOwn (MkTerm tn) (MkTerm n) = term do
    tn' <- intoTokenName <$> tn
    n' <- intoInt <$> n
    pure $ VDiagram $ mempty {diagMint = mempty {valueOwn = Map.singleton tn' n'}}
  witnessMint (MkTerm cs) (MkTerm tn) (MkTerm n) = term do
    cs' <- intoCurrencySymbol <$> cs
    tn' <- intoTokenName <$> tn
    n' <- intoInt <$> n
    pure $ VDiagram $ mempty {diagMint = mempty {valueOther = Map.singleton cs' $ Map.singleton tn' n'}}
  requireSignature (MkTerm pkh) = term do
    pkh' <- intoPubKeyHash <$> pkh
    pure $ VDiagram $ mempty {diagSign = Set.singleton pkh'}
  requireValidRange (MkTerm range) = term do
    range' <- intoTimeRange <$> range
    pure $ VDiagram $ mempty {diagTimeRange = range'}
  toAddress (MkTerm addr) (MkTerm val) (MkTerm dat) = term do
    addr' <- intoAddress <$> addr
    val' <- intoValue <$> val
    dat' <- intoData <$> dat
    pure $ VUTXO $ UTXOAddr addr' val' dat'
  toProtocol p (MkTerm dat) (MkTerm val) = term do
    let name = protocolName p
    val' <- intoValue <$> val
    VUTXO . UTXOProtocol name val' <$> dat
  fromPkh (MkTerm pkh) = term do
    pkh' <- intoPubKeyHash <$> pkh
    pure $ VAddress $ AddrPubKey pkh'
  utxoRefIs (MkTerm ref) (MkTerm utxo) = term do
    ref' <- intoUTXORef <$> ref
    utxo' <- intoUTXO <$> utxo
    pure $ VDiagram $ mempty {diagMap = [(ref', utxo')]}
  emptyValue = pterm $ VValue mempty
  mkValue (MkTerm cs) (MkTerm tn) (MkTerm n) = term do
    cs' <- intoCurrencySymbol <$> cs
    tn' <- intoTokenName <$> tn
    n' <- intoInt <$> n
    pure $ VValue $ mempty {valueOther = Map.singleton cs' $ Map.singleton tn' n'}
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

prettyList :: [Doc a] -> Doc a
prettyList = P.group . P.encloseSep (P.flatAlt "[ " "[") (P.flatAlt " ]" "]") ", " . fmap P.align

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

instance Pretty Val where
  pretty = \case
    VLam _ -> "{λ}"
    VPair (x, y) -> prettyChain [pretty x <> ",", pretty y]
    VEither et -> case et of
      Left x -> prettyCon "Left" [pretty x]
      Right y -> prettyCon "Right" [pretty y]
    VInt n -> pretty n
    VBS bs -> P.dquotes $ pretty $ T.decodeUtf8 bs
    VUnit -> "()"
    VNat n -> pretty n
    VList xs -> prettyList $ fmap pretty xs
    VSOP (_ :: Proxy a) x ->
      let conName =
            collapse_NP
              ( map_NP (SOP.K . SOP.constructorName) $
                  SOP.constructorInfo $
                    SOP.gdatatypeInfo $
                      Proxy @(PConcrete EK a)
              )
              !! index_SOP x
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

--------------------------------------------------------------------------------
-- Compilation -----------------------------------------------------------------
--------------------------------------------------------------------------------

compile :: forall a. IsPType EK a => Term EK a -> (Val, Ty)
compile v =
  let val = runIdentity $ runTerm v
      ty = typeInfo (Proxy @a)
   in (val, ty)

compileShow :: IsPType EK a => Term EK a -> Text
compileShow = docToText . pretty . fst . compile

docToText :: Doc a -> Text
docToText = P.renderStrict . P.layoutPretty P.defaultLayoutOptions {P.layoutPageWidth = P.AvailablePerLine 120 1.0}

toDiagram :: Term EK (PDiagram d) -> Diagram
toDiagram (MkTerm diag) = intoDiagram $ runIdentity diag
