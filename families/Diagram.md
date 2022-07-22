# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Diagram where

import Families (Transaction)
import HKD
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Graph.Inductive (Gr, mkGraph)
--import Data.GraphViz.Types.Graph (mkGraph)

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
  datum :: Text,
  outputCurrencies :: [Currency]}

data Wallet = Wallet {
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text}

transactionTypeDiagram :: TransactionTypeDiagram -> Gr Text Text
transactionTypeDiagram TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes edges where
  nodes = (transactionNode, transactionName) : (isNodes <> osNodes <> iwNodes <> owNodes <> scriptNodes)
  edges = [(n, transactionNode, redeemer $ scriptInputs Map.! name) | (n, name) <- isNodes]
       <> [(n, transactionNode, name) | (n, name) <- iwNodes]
       <> [(transactionNode, n, datum $ scriptOutputs Map.! name) | (n, name) <- osNodes]
       <> [(transactionNode, n, name) | (n, name) <- owNodes]
  transactionNode = 0
  isNodes = [(5*n, name) | (n, (name, InputFromScript {})) <- zip [1..] $ Map.toList scriptInputs]
  osNodes = [(5*n+1, name) | (n, (name, OutputToScript {})) <- zip [1..] $ Map.toList scriptOutputs]
  iwNodes = [(5*n+2, name) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletInputs]
  owNodes = [(5*n+3, name) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletOutputs]
  scriptNodes = [(5*n+4, name) | (n, name) <- zip [1..] $ Map.keys scriptInputs <> Map.keys scriptOutputs]
~~~
