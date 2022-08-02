{-# LANGUAGE DuplicateRecordFields, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}

module Families.Diagram where

import Families (Transaction)
import HKD
import Data.Bifunctor (first)
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

data TransactionTypeDiagram = TransactionTypeDiagram {
  transactionName :: Text,
  scriptInputs :: Map Text InputFromScript,
  scriptOutputs :: Map Text OutputToScript,
  walletInputs :: Map Text Wallet,
  walletOutputs :: Map Text Wallet}

data InputFromScript = InputFromScript {
  fromScript :: Text,
  redeemer :: Text,
  datum :: Text,
  currencies :: [Currency]}

data OutputToScript = OutputToScript {
  toScript :: Text,
  datum :: Text,
  currencies :: [Currency]}

data Wallet = Wallet {
  walletName :: Text,
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text} deriving (Eq, IsString, Show)

nodeType :: Int -> Int
nodeType = (`mod` 7)

isTransaction :: Int -> Bool
isTransaction = (== 0) . nodeType

transactionGraphToDot :: Gr NodeId Text -> DotGraph Int
transactionGraphToDot g = graphToDot params g'
  where g' :: Gr Text Text
        g' = first nodeLabel $ foldr dropNode g (filter ((> 4) . nodeType) $ map fst $ labNodes g) -- drop script and wallet nodes
        dropNode :: Int -> Gr a b -> Gr a b
        dropNode n gr = snd (match n gr)
        params :: GraphvizParams Int Text Text Int Text
        params = defaultParams{
          clusterID = Num . Int,
          clusterBy = clustering,
          fmtCluster = \ n -> case match n g of
            (Just (_, _, node, _), _) -> [GraphAttrs [toLabel $ nodeLabel node]],
          fmtNode = \ (n, l) -> toLabel l : if isTransaction n then [Shape DoubleOctagon] else [],
          -- give wallet inputs weight 0 to move wallet clusters below transaction
          fmtEdge = \ (src, dest, l) ->
            toLabel l : if isTransaction dest && nodeType src == 3 then [Weight $ Int 0] else []}
        clustering :: (Int, Text) -> NodeCluster Int (Int, Text)
        clustering (n, name)
          | (Just (ins, node, _, outs), _) <- match n g =
            case nodeType n of
              -- inputs from scripts
              1 | [(_, script)] <- ins, (Just (_, _, scriptName, _), _) <- match script g ->
                  C script (N (n, name))
              -- outputs from scripts
              2 | [(_, script)] <- filter (not . isTransaction . snd) ins,
                  (Just (_, _, scriptName, _), _) <- match script g ->
                  C script (N (n, name))
              -- inputs from wallets
              3 | [(_, wallet)] <- ins, (Just (_, _, walletName, _), _) <- match wallet g ->
                  C wallet (N (n, name))
              -- outputs from wallets
              4 | [(_, wallet)] <- filter (not . isTransaction . snd) ins,
                  (Just (_, _, walletName, _), _) <- match wallet g ->
                  C wallet (N (n, name))
              -- scripts
              5 -> C n (N (n, name))
              -- wallets
              6 -> C n (N (n, name))
              -- transactions
              _ -> N (n, name)

data OverlayMode = Parallel | Serial

data NodeId = ScriptNamed Text
            | WalletNamed Text
            | TransactionNamed Text
            | ScriptUTxO {script :: Text, datum :: Text, currencies :: [Currency]}
            | WalletUTxO Text [Currency]
            deriving (Eq, Show)

nodeLabel :: NodeId -> Text
nodeLabel (ScriptNamed name) = name
nodeLabel (WalletNamed name) = name
nodeLabel (TransactionNamed name) = name
nodeLabel ScriptUTxO {currencies} = Text.intercalate ", " (currencyName <$> currencies)
nodeLabel (WalletUTxO _ currencies) = Text.intercalate ", " (currencyName <$> currencies)

transactionTypeFamilyGraph :: OverlayMode -> [TransactionTypeDiagram] -> Gr Text Text
transactionTypeFamilyGraph = undefined

transactionTypeGraph :: Int -> TransactionTypeDiagram -> Gr NodeId Text
transactionTypeGraph
  startIndex
  TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes edges where
  nodes =
    (transactionNode, TransactionNamed transactionName)
    : (isNodes <> osNodes <> iwNodes <> owNodes <> scriptNodes <> walletNodes)
  edges = [ (n, transactionNode, redeemer <> "@" <> name)
          | (name, InputFromScript{fromScript, redeemer, datum, currencies}) <- Map.toList scriptInputs,
            let [(n, _)] = filter ((ScriptUTxO fromScript datum currencies ==) . snd) isNodes]
       <> [ (n, transactionNode, name)
          | (name, Wallet w currencies) <- Map.toList walletInputs,
            let [(n, _)] = filter ((WalletUTxO w currencies ==) . snd) iwNodes]
       <> [ (transactionNode, n, name)
          | (name, OutputToScript{toScript, datum, currencies}) <- Map.toList scriptOutputs,
            let [(n, _)] = filter ((ScriptUTxO toScript datum currencies ==) . snd) osNodes]
       <> [ (transactionNode, n, name)
          | (name, Wallet w currencies) <- Map.toList walletOutputs,
            let [(n, _)] = filter ((WalletUTxO w currencies ==) . snd) owNodes]
       <> [ (scriptNode, n, "")
          | (n, ScriptUTxO name _ _) <- isNodes,
            let [(scriptNode, _)] = filter ((ScriptNamed name ==) . snd) scriptNodes]
       <> [ (scriptNode, n, "")
          | (n, ScriptUTxO name _ _) <- osNodes,
            let [(scriptNode, _)] = filter ((ScriptNamed name ==) . snd) scriptNodes]
       <> [ (walletNode, n, "")
          | (n, WalletUTxO name _) <- iwNodes <> owNodes,
            let [(walletNode, _)] = filter ((WalletNamed name ==) . snd) walletNodes]
  transactionNode = 7*startIndex
  isNodes =
    [ (7*n+1, ScriptUTxO fromScript datum currencies)
    | (n, (name, InputFromScript {fromScript, datum, currencies})) <- zip [startIndex..] $ Map.toList scriptInputs ]
  osNodes =
    [ (7*n+2, ScriptUTxO toScript datum currencies)
    | (n, (name, OutputToScript {toScript, datum, currencies})) <- zip [startIndex..] $ Map.toList scriptOutputs]
  iwNodes =
    [ (7*n+3, WalletUTxO walletName currencies)
    | (n, (name, Wallet {walletName, currencies})) <- zip [startIndex..] $ Map.toList walletInputs]
  owNodes =
    [ (7*n+4, WalletUTxO walletName currencies)
    | (n, (name, Wallet {walletName, currencies})) <- zip [startIndex..] $ Map.toList walletOutputs]
  scriptNodes =
    [ (7*n+5, ScriptNamed name)
    | (n, name) <- zip [0..] $ nub $ map snd $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
  walletNodes =
    [ (7*n+6, WalletNamed name)
    | (n, name) <- zip [0..] $ nub $ map snd $ Map.toList (walletName <$> walletInputs <> walletOutputs) ]
