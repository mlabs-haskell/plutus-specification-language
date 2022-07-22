# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Diagram where

import Families (Transaction)
import HKD
import Data.Kind (Type)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.GraphViz (DotGraph, GraphvizParams, GraphvizOutput (Plain), defaultParams, graphToDot, preview, runGraphviz)

main = do
  preview (transactionTypeGraph exchangeTypeDiagram)
  runGraphviz exchangeDot Plain "exchange.gv"
  putStrLn "Done"

exchangeDot :: DotGraph Int
exchangeDot = graphToDot params (transactionTypeGraph exchangeTypeDiagram)
  where params :: GraphvizParams Int Text Text () Text
        params = defaultParams

exchangeTypeDiagram :: TransactionTypeDiagram
exchangeTypeDiagram = TransactionTypeDiagram {
  transactionName = "exchange",
  scriptInputs = Map.fromList [
    ("exchange", InputFromScript "CentralExchange" "()"),
    ("oracle1", InputFromScript "Oracle 1" "Trade"),
    ("oracle2", InputFromScript "Oracle 2" "Trade")],
  walletInputs = Map.fromList [
    ("wallet1", Wallet []),
    ("wallet2", Wallet [])],
  scriptOutputs = Map.fromList [
    ("exchange", OutputToScript "CentralExchange"),
    ("oracle1", OutputToScript "Oracle 1"),
    ("oracle2", OutputToScript "Oracle 2")],
  walletOutputs = Map.fromList [
    ("wallet1", Wallet []),
    ("wallet2", Wallet [])]}

data TransactionTypeDiagram = TransactionTypeDiagram {
  transactionName :: Text,
  scriptInputs :: Map Text InputFromScript,
  scriptOutputs :: Map Text OutputToScript,
  walletInputs :: Map Text Wallet,
  walletOutputs :: Map Text Wallet}

data InputFromScript = InputFromScript {
  fromScript :: Text,
  redeemer :: Text}
--  inputCurrencies :: [Currency]}

data OutputToScript = OutputToScript {
  toScript :: Text}
--  datum :: Text,
--  outputCurrencies :: [Currency]}

data Wallet = Wallet {
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text}

transactionTypeGraph :: TransactionTypeDiagram -> Gr Text Text
transactionTypeGraph TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes edges where
  nodes = (transactionNode, transactionName) : (isNodes <> osNodes <> iwNodes <> owNodes <> scriptNodes)
  edges = [(n, transactionNode, redeemer $ scriptInputs Map.! name) | (n, name) <- isNodes]
       <> [(n, transactionNode, name) | (n, name) <- iwNodes]
       <> [(transactionNode, n, name) | (n, name) <- osNodes]
       <> [(transactionNode, n, name) | (n, name) <- owNodes]
       <> [ (scriptNode, n, "")
          | (n, name) <- isNodes,
            let scriptName = fromScript $ scriptInputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
       <> [ (n, scriptNode, "")
          | (n, name) <- osNodes,
            let scriptName = toScript $ scriptOutputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
  transactionNode = 0
  isNodes = [(5*n, name) | (n, (name, InputFromScript {})) <- zip [1..] $ Map.toList scriptInputs]
  osNodes = [(5*n+1, name) | (n, (name, OutputToScript {})) <- zip [1..] $ Map.toList scriptOutputs]
  iwNodes = [(5*n+2, name) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletInputs]
  owNodes = [(5*n+3, name) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletOutputs]
  scriptNodes =
    [ (5*n+4, name)
    | (n, name) <- zip [1..] $ nub $ map snd $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
~~~
