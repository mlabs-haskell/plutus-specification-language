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
  mermaidDiagram,
  maks,
  counter,
  example,
) where

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import PSL hiding (counter)
import PSL.Eval.Backend
import PSL.Eval.Interval (Interval)
import PSL.Eval.Mermaid (mermaidDiagram)
import PSL.Eval.Tx
import Plutarch.Core

mkPkh :: ByteString -> Term EK PPubKeyHash
mkPkh bs = pterm $ VPubKeyHash $ PubKeyHash bs

mkCurrencySymbol :: ByteString -> Term EK PCurrencySymbol
mkCurrencySymbol bs = pterm $ VCurrencySymbol $ CurrencySymbol bs

mkUTXORef :: ByteString -> Term EK PUTXORef
mkUTXORef bs = pterm $ VUTXORef $ UTXORef bs

mkTokenName :: ByteString -> Term EK PTokenName
mkTokenName bs = pterm $ VTokenName $ TokenName bs

mkTimeRange :: Interval POSIXTime -> Term EK PTimeRange
mkTimeRange iv = pterm $ VTimeRange $ fmap EvalTime iv

maks :: Term EK (PDiagram MaksDatum)
maks = maksCases . pcon $ MaksFork 1 2

counter :: Term EK (PDiagram CounterDatum)
counter =
  counterCases . pcon $
    CounterConsume
      (fromPkh $ mkPkh "cnu328190ru389ndcuic")
      (pcon $ PDataList $ pcon PNil)
      (mkOwnValue (mkTokenName "tok") 100)

example :: Term EK (PDiagram ExampleDatum)
example =
  exampleCases . pcon $
    ExampleConsume
      10
      (mkOwnValue (mkTokenName "tok") 100)
      (mkOwnValue (mkTokenName "tok'") 100)
      (mkPkh "cnu328")
      (mkUTXORef "j34fi9")
