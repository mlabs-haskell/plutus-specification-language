# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Diagram where

import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Families.Diagram (
  TransactionTypeDiagram (
    TransactionTypeDiagram, transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs),
  InputFromScript (InputFromScript, currencies, datum, redeemer, fromScript),
  OutputToScript (OutputToScript, currencies, datum, toScript),
  Wallet (Wallet),
  OverlayMode (Distinct, Parallel, Serial),
  transactionGraphToDot, transactionTypeGraph, transactionTypeFamilyGraph)
import Data.GraphViz (
  GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  runGraphviz, runGraphvizCanvas')
import qualified Language.Haskell.TH as TH
import Families.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType)
import qualified HKD

e1 = $( [t| 'HKD.Exchange 1 2 |] >>= untypedDiagramForTransactionType)
d1 = $$(diagramForTransactionType $ TH.PromotedT $ TH.mkName "HKD.DrainCollectedFees")

main = do
  let g1 = transactionGraphToDot "exchange" (transactionTypeGraph 0 exchange)
      g2 = transactionGraphToDot "distinct" (transactionTypeFamilyGraph Distinct [updateOracle, exchange, drain])
      g3 = transactionGraphToDot "parallel" (transactionTypeFamilyGraph Parallel [updateOracle, exchange, drain])
      g4 = transactionGraphToDot "serial" (transactionTypeFamilyGraph Serial [updateOracle, exchange, drain])
  forkIO (runGraphvizCanvas' g2 Xlib)
  forkIO (runGraphvizCanvas' g3 Xlib)
  forkIO (runGraphvizCanvas' g4 Xlib)
  runGraphviz g4 Canon "update-exchange-drain.dot"

updateOracle :: TransactionTypeDiagram
updateOracle = TransactionTypeDiagram {
  transactionName = "updateOracle",
  scriptInputs = Map.fromList [
    ("oracle",
     InputFromScript {fromScript = "Oracle 1", redeemer = "Update", datum = "OracleDatum", currencies = ["Token1"]})],
  scriptOutputs = Map.fromList [
    ("oracle",
     OutputToScript {toScript = "Oracle 1", datum = "OracleDatum", currencies = ["Token1"]})],
  walletInputs = mempty,
  walletOutputs = mempty}

exchange :: TransactionTypeDiagram
exchange = TransactionTypeDiagram {
  transactionName = "exchange",
  scriptInputs = Map.fromList [
    ("exchange", InputFromScript {fromScript = "CentralExchange", redeemer = "()", datum = "()", currencies = []}),
    ("oracle1",
     InputFromScript {fromScript = "Oracle 1", redeemer = "Trade", datum = "OracleDatum", currencies = ["Token1"]}),
    ("oracle2",
     InputFromScript {fromScript = "Oracle 2", redeemer = "Trade", datum = "OracleDatum", currencies = ["Token2"]})],
  walletInputs = Map.fromList [
    ("wallet1", Wallet "Wallet 1" ["Token1"]),
    ("wallet2", Wallet "Wallet 2" ["Token2"])],
  scriptOutputs = Map.fromList [
    ("exchange", OutputToScript {toScript = "CentralExchange", datum = "()", currencies = ["Token1", "Token2"]}),
    ("oracle1", OutputToScript {toScript = "Oracle 1", datum = "OracleDatum", currencies = ["Token1"]}),
    ("oracle2", OutputToScript {toScript = "Oracle 2", datum = "OracleDatum", currencies = ["Token2"]})],
  walletOutputs = Map.fromList [
    ("wallet1", Wallet "Wallet 1" ["Token2"]),
    ("wallet2", Wallet "Wallet 2" ["Token1"])]}

drain :: TransactionTypeDiagram
drain = TransactionTypeDiagram {
  transactionName = "drainCollectedFees",
  scriptInputs = Map.fromList [
    ("exchange",
     InputFromScript {fromScript = "CentralExchange", redeemer = "()", datum = "()", currencies = ["Token1", "Token2"]})],
  scriptOutputs = Map.fromList [
    ("exchange",
     OutputToScript {toScript = "CentralExchange", datum = "()", currencies = []})],
  walletInputs = mempty,
  walletOutputs = Map.fromList [
    ("authority",
     Wallet "Authority" ["Token1", "Token2"])]}
~~~
