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
  count,
  example,
  fun,
  ident,
  mermaidDiagram,
  toDecisionTree,
  DecisionTree (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import PSL hiding (counter)
import PSL.Eval.Backend
import PSL.Eval.Interval (Interval)
import PSL.Eval.Mermaid (mermaidDiagram)
import Plutarch.Core
import Plutarch.Frontends.Data
import Plutarch.Prelude

-- | Inject a PubKeyHash into the script.
mkPkh :: ByteString -> Term EK PPubKeyHash
mkPkh bs = pterm $ VPubKeyHash $ PubKeyHash bs

-- | Inject a CurrencySymbol into the script.
mkCurrencySymbol :: ByteString -> Term EK PCurrencySymbol
mkCurrencySymbol bs = pterm $ VCurrencySymbol $ CurrencySymbol bs

-- | Inject a UTXORef into the script.
mkUTXORef :: ByteString -> Term EK PUTXORef
mkUTXORef bs = pterm $ VUTXORef $ UTXORef bs

-- | Inject a TokenName into the script.
mkTokenName :: ByteString -> Term EK PTokenName
mkTokenName bs = pterm $ VTokenName $ TokenName $ BS bs

-- | Inject a TimeRange into the script.
mkTimeRange :: Interval POSIXTime -> Term EK PTimeRange
mkTimeRange iv = pterm $ VTimeRange $ fmap EvalTime iv

maks :: Term EK (PDiagram MaksDatum)
maks = maksCases (ident "pkh") (ident "datum")

count :: Term EK (PDiagram CounterDatum)
count = counterCases (ident "datum")

example :: Term EK (PDiagram ExampleDatum)
example =
  exampleCases (ident "datum")

fun :: Term EK (PInteger #-> PInteger)
fun = plam \x -> x + 1 * 2

-- | Inject a metavariable into the script.
ident :: Text -> Term EK a
ident nm = pterm $ vid $ IdentM nm
