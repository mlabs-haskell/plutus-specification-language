{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PSL.Eval (
  EK,
  TypeReprInfo,
  typeReprInfo,
  TypeInfo,
  typeInfo,
  compile,
  compileShow,
  docToText,
  mkPkh,
  mkCurrencySymbol,
  mkUTXORef,
  mkTokenName,
  mkTimeRange,
  maks,
  counter,
  example,
  fun,
  ident,
  mermaidDiagram,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import PSL hiding (counter)
import PSL.Eval.Backend
import PSL.Eval.Interval (Interval)
import PSL.Eval.Mermaid (mermaidDiagram)
import Plutarch.Core
import Plutarch.Lam (plam)

mkPkh :: ByteString -> Term EK PPubKeyHash
mkPkh bs = pterm $ VPubKeyHash $ PubKeyHash bs

mkCurrencySymbol :: ByteString -> Term EK PCurrencySymbol
mkCurrencySymbol bs = pterm $ VCurrencySymbol $ CurrencySymbol bs

mkUTXORef :: ByteString -> Term EK PUTXORef
mkUTXORef bs = pterm $ VUTXORef $ UTXORef bs

mkTokenName :: ByteString -> Term EK PTokenName
mkTokenName bs = pterm $ VTokenName $ TokenName $ BS bs

mkTimeRange :: Interval POSIXTime -> Term EK PTimeRange
mkTimeRange iv = pterm $ VTimeRange $ fmap EvalTime iv

maks :: Term EK (PDiagram MaksDatum)
maks = maksCases . pcon $ MaksFork 1 2

counter :: Term EK (PDiagram CounterDatum)
counter =
  counterCases . pcon $
    CounterConsume
      (ident "pkh")
      (ident "dat")
      (ident "value")

example :: Term EK (PDiagram ExampleDatum)
example =
  exampleCases . pcon $
    ExampleConsume
      (ident "x")
      (mkOwnValue (ident "tok1") (ident "value1"))
      (ident "myValue")
      (ident "pkh")
      (ident "utxo")

fun :: Term EK (PInteger #-> PInteger)
fun = plam \x -> x + 1 * 2

ident :: Text -> Term EK a
ident nm = pterm $ vid $ Ident 0 nm