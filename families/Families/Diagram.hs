{-# LANGUAGE BangPatterns, DuplicateRecordFields, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}

module Families.Diagram where

import Families (Transaction)
import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)
import Data.Kind (Type)
import Data.List (foldl', nub, sortOn)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Ord (Down (Down))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Graph.Inductive (Context, Gr, empty, gmap, labEdges, labNodes, match, mkGraph)
import Data.GraphViz (
  DotGraph, GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  GlobalAttributes (GraphAttrs),
  GraphvizParams(clusterBy, clusterID, fmtCluster, fmtEdge, fmtNode, globalAttributes, isDotCluster),
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

newtype Currency = Currency {currencyName :: Text} deriving (Eq, Ord, IsString, Show)

nodeType :: Int -> Int
nodeType = (`mod` 7)

isTransaction :: Int -> Bool
isTransaction = (== 0) . nodeType

transactionGraphToDot :: Text -> Gr NodeId Text -> DotGraph Int
transactionGraphToDot caption g = graphToDot params g' where
  g' :: Gr Text Text
  g' = first nodeLabel $ foldr dropNode g (filter ((> 4) . nodeType) $ map fst $ labNodes g)
  -- drop script and wallet nodes
  dropNode :: Int -> Gr a b -> Gr a b
  dropNode n gr = snd (match n gr)
  params :: GraphvizParams Int Text Text Int Text
  params = defaultParams{
    globalAttributes = [GraphAttrs [toLabel caption]],
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
    | (Just (ins, node, _, _outs), _) <- match n g, let noCluster = N (n, name) =
      case nodeType n of
        -- inputs from scripts
        1 | [(_, script)] <- nub $ filter notToTransaction ins -> C script noCluster
        -- outputs from scripts
        2 | [(_, script)] <- nub $ filter notToTransaction ins -> C script noCluster
        -- inputs from wallets
        3 | [(_, wallet)] <- ins -> C wallet noCluster
        -- outputs from wallets
        4 | [(_, wallet)] <- filter notToTransaction ins -> C wallet noCluster
        -- scripts
        5 -> C n noCluster
        -- wallets
        6 -> C n noCluster
        -- transactions
        _ -> noCluster
  notToTransaction :: (Text, Int) -> Bool
  notToTransaction = not . isTransaction . snd

data OverlayMode = Distinct | Parallel | Serial deriving (Eq, Ord, Show)

data NodeId = ScriptNamed Text
            | WalletNamed Text
            | TransactionNamed Text
            | ScriptUTxO {script :: Text, datum :: Text, currencies :: [Currency]}
            | WalletUTxO Text [Currency]
            deriving (Eq, Ord, Show)

nodeLabel :: NodeId -> Text
nodeLabel (ScriptNamed name) = name
nodeLabel (WalletNamed name) = name
nodeLabel (TransactionNamed name) = name
nodeLabel ScriptUTxO {currencies} = Text.intercalate ", " (currencyName <$> currencies)
nodeLabel (WalletUTxO _ currencies) = Text.intercalate ", " (currencyName <$> currencies)

transactionTypeFamilyGraph :: OverlayMode -> [TransactionTypeDiagram] -> Gr NodeId Text
transactionTypeFamilyGraph mode = mergeGraphs . foldl' addTx (0, []) where
  mergeGraphs :: (Int, [Gr NodeId Text]) -> Gr NodeId Text
  mergeGraphs (total, gs) = replaceFixedNodes mode total g
    where g = gconcat gs
  addTx :: (Int, [Gr NodeId Text]) -> TransactionTypeDiagram -> (Int, [Gr NodeId Text])
  addTx (!total, gs) d = (total + maxNodeCount d, transactionTypeGraph total d : gs)
  maxNodeCount :: TransactionTypeDiagram -> Int
  maxNodeCount TransactionTypeDiagram{scriptInputs, scriptOutputs, walletInputs, walletOutputs} =
    1 `max` (length scriptInputs + length scriptOutputs) `max` (length walletInputs + length walletOutputs)

replaceFixedNodes :: OverlayMode -> Int -> Gr NodeId Text -> Gr NodeId Text
replaceFixedNodes mode total g =
  mkGraph (sortOn (Down . fst) $ first switchNode <$> labNodes g) (switchEnds <$> labEdges g)
  where g' = replaceFixedNodes Distinct total g
        switchEnds (start, end, label) = (switchNode start, switchNode end, label)
        switchNode n = IntMap.findWithDefault n n nodeMap
        nodeIdMap :: Map NodeId Int
        nodeIdMap = case mode of
          Parallel -> foldMap nodeMapOfType [1..6]
          _ -> foldMap nodeMapOfType [5..6]
        nodeMapOfType :: Int -> Map NodeId Int
        nodeMapOfType n = Map.fromList (zip (nodesOfType n g) $ (7*total+) <$> [n, n+7 ..])
        nodesOfType :: Int -> Gr NodeId Text -> [NodeId]
        nodesOfType t g = snd <$> filter ((t ==) . nodeType . fst) (labNodes g)
        nodeMap :: IntMap Int
        nodeMap = case mode of
          Serial -> IntMap.fromList $ mapMaybe targetUTxO $ labNodes g
          _ -> IntMap.fromList $ mapMaybe targetNode $ labNodes g
        targetNode :: (Int, NodeId) -> Maybe (Int, Int)
        targetNode (n, node) = (,) n <$> Map.lookup node nodeIdMap
        targetUTxO :: (Int, NodeId) -> Maybe (Int, Int)
        targetUTxO (n, node)
          | nodeType n `elem` [1, 3],
            (Just ([(_, address)], _, _, [(_, trans)]), _) <- match n g',
            (Just ([], _, _, utxos), _) <- match address g',
            _:(_, n'):_ <- takeWhile ((n /=) . snd) $ sortOn snd utxos,
            nodeType n' `elem` [2, 4],
            (Just (ins, _, node', []), _) <- match n' g',
            node' == node,
            [(_, trans')] <- filter ((/= address) . snd) ins,
            trans' < trans =
              Just (n, n')
          | nodeType n `elem` [5, 6] = (,) n <$> Map.lookup node nodeIdMap
          | otherwise = Nothing

gconcat :: [Gr a b] -> Gr a b
gconcat = uncurry mkGraph . mconcat . map (labNodes &&& labEdges)

transactionTypeGraph :: Int -> TransactionTypeDiagram -> Gr NodeId Text
transactionTypeGraph
  startIndex
  TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs}
  = mkGraph nodes (nub edges) where
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
          | (n, ScriptUTxO name _ _) <- isNodes <> osNodes,
            let [(scriptNode, _)] = filter ((ScriptNamed name ==) . snd) scriptNodes]
       <> [ (walletNode, n, "")
          | (n, WalletUTxO name _) <- iwNodes <> owNodes,
            let [(walletNode, _)] = filter ((WalletNamed name ==) . snd) walletNodes]
  transactionNode = 7*startIndex
  isNodes =
    [ (7*n+1, ScriptUTxO fromScript datum currencies)
    | (n, (name, InputFromScript {fromScript, datum, currencies})) <- zip [startIndex..] $ Map.toList scriptInputs]
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
    | (n, name) <- zip [startIndex..] $ nub $ map snd $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
  walletNodes =
    [ (7*n+6, WalletNamed name)
    | (n, name) <- zip [startIndex..] $ nub $ map snd $ Map.toList (walletName <$> walletInputs <> walletOutputs) ]
