# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Diagram where

import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Families.Diagram (
  TransactionTypeDiagram (
    TransactionTypeDiagram, transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs),
  InputFromScript (InputFromScript),
  OutputToScript (OutputToScript),
  Wallet (Wallet),
  transactionGraphToDot, transactionTypeGraph)
import Data.GraphViz (
  GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  runGraphviz, runGraphvizCanvas')

main = do
  let g = transactionGraphToDot (transactionTypeGraph exchangeTypeDiagram)
  forkIO (runGraphvizCanvas' g Xlib)
  runGraphviz g Canon "exchange.dot"

exchangeTypeDiagram :: TransactionTypeDiagram
exchangeTypeDiagram = TransactionTypeDiagram {
  transactionName = "exchange",
  scriptInputs = Map.fromList [
    ("exchange", InputFromScript "CentralExchange" "()" []),
    ("oracle1", InputFromScript "Oracle 1" "Trade" ["Token1"]),
    ("oracle2", InputFromScript "Oracle 2" "Trade" ["Token2"])],
  walletInputs = Map.fromList [
    ("wallet1", Wallet "Wallet 1" ["Token1"]),
    ("wallet2", Wallet "Wallet 2" ["Token2"])],
  scriptOutputs = Map.fromList [
    ("exchange", OutputToScript "CentralExchange" ["Token1", "Token2"]),
    ("oracle1", OutputToScript "Oracle 1" ["Token1"]),
    ("oracle2", OutputToScript "Oracle 2" ["Token2"])],
  walletOutputs = Map.fromList [
    ("wallet1", Wallet "Wallet 1" ["Token2"]),
    ("wallet2", Wallet "Wallet 2" ["Token1"])]}
~~~
