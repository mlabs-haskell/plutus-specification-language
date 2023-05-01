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
  payment,
  fun,
  ident,
  mermaidDiagram,
  toDecisionTree,
  DecisionTree (..),
  prettyDiagram,
) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import PSL hiding (counter)
import PSL.Eval.Backend
import PSL.Eval.Interval (Interval)
import PSL.Eval.Mermaid (mermaidDiagram)
import Plutarch.Core
import Plutarch.Frontends.Data
import Plutarch.Prelude
import Prettyprinter (Pretty (pretty))

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

-- | Inject a metavariable into the script.
ident :: Text -> Term EK a
ident nm = pterm $ vid $ IdentM nm

--- Examples ---

maks :: Term EK (PDiagram MaksDatum)
maks = maksCases (ident "pkh") (ident "redeemer")

count :: Term EK (PDiagram CounterDatum)
count = counterCases (ident "redeemer")

payment :: Term EK (PDiagram PPubKeyHash)
payment = paymentCases (ident "pkh")

fun :: Term EK (PInteger #-> PInteger)
fun = plam \x -> x + 1 * 2

prettyDiagram :: IsPType EK d => Term EK (PDiagram d) -> Text
prettyDiagram x = docToText $ pretty $ fmap mermaidDiagram $ fromJust $ toDecisionTree x
