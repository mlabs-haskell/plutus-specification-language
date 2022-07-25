# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}
module Diagram where

import Families (Transaction)
import HKD
import Data.Kind (Type)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (toList)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
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
    ("exchange", InputFromScript "CentralExchange" "()" []),
    ("oracle1", InputFromScript "Oracle 1" "Trade" ["Token1"]),
    ("oracle2", InputFromScript "Oracle 2" "Trade" ["Token2"])],
  walletInputs = Map.fromList [
    ("wallet1", Wallet ["Token1"]),
    ("wallet2", Wallet ["Token2"])],
  scriptOutputs = Map.fromList [
    ("exchange", OutputToScript "CentralExchange" ["Token1", "Token2"]),
    ("oracle1", OutputToScript "Oracle 1" ["Token1"]),
    ("oracle2", OutputToScript "Oracle 2" ["Token2"])],
  walletOutputs = Map.fromList [
    ("wallet1", Wallet ["Token2"]),
    ("wallet2", Wallet ["Token1"])]}

data TransactionTypeDiagram = TransactionTypeDiagram {
  transactionName :: Text,
  scriptInputs :: Map Text InputFromScript,
  scriptOutputs :: Map Text OutputToScript,
  walletInputs :: Map Text Wallet,
  walletOutputs :: Map Text Wallet}

data InputFromScript = InputFromScript {
  fromScript :: Text,
  redeemer :: Text,
  inputCurrencies :: [Currency]}

data OutputToScript = OutputToScript {
  toScript :: Text,
--  datum :: Text,
  outputCurrencies :: [Currency]}

data Wallet = Wallet {
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text} deriving (Eq, IsString, Show)

present :: [Currency] -> Text
present = Text.intercalate ", " . map currencyName

transactionTypeGraph :: TransactionTypeDiagram -> Gr Text Text
transactionTypeGraph TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes edges where
  nodes =
    (transactionNode, transactionName)
    : (map dropMiddle (isNodes <> osNodes <> iwNodes <> owNodes) <> scriptNodes)
  edges = [(n, transactionNode, redeemer $ scriptInputs Map.! name) | (n, name, _) <- isNodes]
       <> [(n, transactionNode, name) | (n, name, _) <- iwNodes]
       <> [(transactionNode, n, name) | (n, name, _) <- osNodes]
       <> [(transactionNode, n, name) | (n, name, _) <- owNodes]
       <> [ (scriptNode, n, "")
          | (n, name, _) <- isNodes,
            let scriptName = fromScript $ scriptInputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
       <> [ (n, scriptNode, "")
          | (n, name, _) <- osNodes,
            let scriptName = toScript $ scriptOutputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
  transactionNode = 0
  isNodes = [(5*n, name, present inputCurrencies) | (n, (name, InputFromScript {inputCurrencies})) <- zip [1..] $ Map.toList scriptInputs]
  osNodes = [(5*n+1, name, present outputCurrencies) | (n, (name, OutputToScript {outputCurrencies})) <- zip [1..] $ Map.toList scriptOutputs]
  iwNodes = [(5*n+2, name, present currencies) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletInputs]
  owNodes = [(5*n+3, name, present currencies) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletOutputs]
  scriptNodes =
    [ (5*n+4, name)
    | (n, name) <- zip [1..] $ nub $ map snd $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
  dropMiddle (a, _, b) = (a, b)
~~~
