# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification,
             PolyKinds, StandaloneKindSignatures #-}
module Diagram where

import Families (Transaction)
import HKD
import Data.Kind (Type)
import Data.Map (Map)
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

data Wallet = InputFromScriptWallet {
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text}

type W1 :: (k1 -> k2 -> Type) -> k1 -> Type
type W2 :: (k1 -> k2 -> Type) -> k2 -> Type
data W1 t a1 = forall a2. W1 (t a1 a2)
data W2 t a2 = forall a1. W2 (t a1 a2)

transactionTypeDiagram :: forall t. (Transaction t) => Gr [Char] [Char]
transactionTypeDiagram = mkGraph nodes edges where
  nodes = (transactionNode, "transaction") : (inputNodes <> outputNodes)
  edges = [(-n, transactionNode, "input") | n <- inputNodes]
       <> [(transactionNode, n, "output") | n <- outputNodes]
  transactionNode = 0
  inputNodes = []
  outputNodes = [] -- hpure @(Outputs t)
~~~
