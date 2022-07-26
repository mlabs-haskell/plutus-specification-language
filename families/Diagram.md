# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}
module Diagram where

import Control.Concurrent (forkIO)
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
import Data.Graph.Inductive (Gr, labNodes, match, mkGraph)
import Data.GraphViz (
  DotGraph, GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  GlobalAttributes (GraphAttrs),
  GraphvizParams(clusterBy, clusterID, fmtCluster, fmtEdge, fmtNode, isDotCluster),
  NodeCluster (C, N), GraphID (Num), Number (Int),
  defaultParams, graphToDot, preview, runGraphviz, runGraphvizCanvas', toLabel)
import Data.GraphViz.Attributes.Complete (Attribute (Shape, Weight), Shape (DoubleOctagon))

main = do
  let g = exchangeDot
  forkIO (runGraphvizCanvas' g Xlib)
  runGraphviz exchangeDot Canon "exchange.dot"
  putStrLn "Done"

exchangeDot :: DotGraph Int
exchangeDot = graphToDot params g'
  where g, g' :: Gr Text Text
        g = transactionTypeGraph exchangeTypeDiagram
        g' = foldr dropNode g (filter ((> 3) . (`mod` 6)) $ map fst $ labNodes g)
        dropNode :: Int -> Gr Text Text -> Gr Text Text
        dropNode n gr = snd (match n gr)
        params :: GraphvizParams Int Text Text Int Text
        params = defaultParams{
          clusterID = Num . Int,
          clusterBy = clustering,
          fmtCluster = \ n -> case match n g of
            (Just (_, _, scriptName, _), _) -> [GraphAttrs [toLabel scriptName]],
          fmtNode = \ (n, l) -> toLabel l : if n == 0 then [Shape DoubleOctagon] else [],
          fmtEdge = \ (src, dest, l) -> toLabel l : if dest == 0 && src `mod` 6 == 2 then [Weight $ Int 0] else []}
        clustering :: (Int, Text) -> NodeCluster Int (Int, Text) 
        clustering (n, name)
          | (Just (ins, node, _, outs), _) <- match n g =
            case n `mod` 6 of
              -- inputs from scripts
              0 | [(_, script)] <- ins, (Just (_, _, scriptName, _), _) <- match script g ->
                  C script (N (n, name))
              -- outputs from scripts
              1 | [(_, script)] <- filter ((/= 0) . snd) ins, (Just (_, _, scriptName, _), _) <- match script g ->
                  C script (N (n, name))
              -- inputs from wallets
              2 | [(_, wallet)] <- ins, (Just (_, _, walletName, _), _) <- match wallet g ->
                  C wallet (N (n, name))
              -- outputs from wallets
              3 | [(_, wallet)] <- filter ((/= 0) . snd) ins, (Just (_, _, walletName, _), _) <- match wallet g ->
                  C wallet (N (n, name))
              -- scripts
              4 -> C n (N (n, name))
              -- wallets
              5 -> C n (N (n, name))
              _ -> N (n, name)

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
  walletName :: Text,
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text} deriving (Eq, IsString, Show)

present :: [Currency] -> Text
present = Text.intercalate ", " . map currencyName

transactionTypeGraph :: TransactionTypeDiagram -> Gr Text Text
transactionTypeGraph TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes edges where
  nodes =
    (transactionNode, transactionName)
    : (map dropMiddle (isNodes <> osNodes <> iwNodes <> owNodes) <> scriptNodes <> walletNodes)
  edges = [(n, transactionNode, redeemer $ scriptInputs Map.! name) | (n, name, _) <- isNodes]
       <> [(n, transactionNode, name) | (n, name, _) <- iwNodes]
       <> [(transactionNode, n, name) | (n, name, _) <- osNodes]
       <> [(transactionNode, n, name) | (n, name, _) <- owNodes]
       <> [ (scriptNode, n, "")
          | (n, name, _) <- isNodes,
            let scriptName = fromScript $ scriptInputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
       <> [ (scriptNode, n, "")
          | (n, name, _) <- osNodes,
            let scriptName = toScript $ scriptOutputs Map.! name;
                [(scriptNode, _)] = filter ((scriptName ==) . snd) scriptNodes]
       <> [ (walletNode, n, "")
          | (n, name, _) <- iwNodes <> owNodes,
            let wName = walletName $ walletInputs Map.! name;
                [(walletNode, _)] = filter ((wName ==) . snd) walletNodes]
  transactionNode = 0
  isNodes = [(6*n, name, present inputCurrencies) | (n, (name, InputFromScript {inputCurrencies})) <- zip [1..] $ Map.toList scriptInputs]
  osNodes = [(6*n+1, name, present outputCurrencies) | (n, (name, OutputToScript {outputCurrencies})) <- zip [1..] $ Map.toList scriptOutputs]
  iwNodes = [(6*n+2, name, present currencies) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletInputs]
  owNodes = [(6*n+3, name, present currencies) | (n, (name, Wallet {currencies})) <- zip [1..] $ Map.toList walletOutputs]
  scriptNodes =
    [ (6*n+4, name)
    | (n, name) <- zip [1..] $ nub $ map snd $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
  walletNodes = 
    [ (6*n+5, name)
    | (n, name) <- zip [1..] $ nub $ map snd $ Map.toList (walletName <$> walletInputs <> walletOutputs) ]
  dropMiddle (a, _, b) = (a, b)
~~~
