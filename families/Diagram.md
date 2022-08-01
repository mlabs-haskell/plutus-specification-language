# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}
module Diagram where

import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Families.Diagram (
  TransactionTypeDiagram (
    TransactionTypeDiagram, transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs),
  InputFromScript (InputFromScript, currencies, datum, redeemer, fromScript),
  OutputToScript (OutputToScript, currencies, datum, toScript),
  Wallet (Wallet),
  transactionGraphToDot, transactionTypeGraph)
import Data.GraphViz (
  GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  runGraphviz, runGraphvizCanvas')

main = do
  let g = transactionGraphToDot (transactionTypeGraph 0 exchangeTypeDiagram)
  forkIO (runGraphvizCanvas' g Xlib)
  runGraphviz g Canon "exchange.dot"

exchangeTypeDiagram :: TransactionTypeDiagram
exchangeTypeDiagram = TransactionTypeDiagram {
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
~~~
