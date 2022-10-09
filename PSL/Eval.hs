{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Eval where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Plutarch.Core
import Plutarch.PType

data Val
  = VLam (Val -> Val) -- \x. M
  | VPair Val Val -- (x, y)
  | VLeft Val -- Left x
  | VRight Val -- Right x

data Ty
  = TFun Ty Ty -- A -> B
  | TProd Ty Ty -- (A, B)
  | TSum Ty Ty -- A | B

intoLam :: Val -> Val -> Val
intoLam = \case
  VLam lam -> lam
  _ -> error "absurd: a function is not a lambda"
intoEither :: Val -> Either Val Val
intoEither = \case
  VLeft l -> Left l
  VRight r -> Right r
  _ -> error "absurd: a sum is not an either"
intoPair :: Val -> (Val, Val)
intoPair = \case
  VPair l r -> (l, r)
  _ -> error "absurd: a product is not a pair"

toHex :: ByteString -> String
toHex bs = do
  x <- BS.unpack bs
  fmap (intToDigit . fromIntegral) [x `div` 16, x `mod` 16]

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

{-# COMPLETE MkTerm #-}
pattern MkTerm :: forall a. EvalM Val -> Term EK a
pattern MkTerm {runTerm} <- (Term (Eval runTerm))

term :: EvalM Val -> Term EK a
term x = Term (Eval x)

pterm :: Val -> Term EK a
pterm t = term $ pure t

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
  pconImpl (PLeft (MkTerm x)) = Eval $ VLeft <$> x
  pconImpl (PRight (MkTerm x)) = Eval $ VRight <$> x
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
