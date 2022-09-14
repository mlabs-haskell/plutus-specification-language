{-# LANGUAGE BangPatterns, DuplicateRecordFields, GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}

module Family.Diagram where

import Family (Transaction, MintQuantity (Burn, BurnSome, Mint, MintOrBurnSome, MintSome))
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
import Data.Graph.Inductive (Context, Gr, empty, gmap, lab, labEdges, labNodes, match, mkGraph)
import Data.GraphViz (
  DotGraph, GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  GlobalAttributes (GraphAttrs),
  GraphvizParams(clusterBy, clusterID, fmtCluster, fmtEdge, fmtNode, globalAttributes, isDotCluster),
  NodeCluster (C, N), GraphID (Num), Number (Int),
  defaultParams, graphToDot, preview, runGraphviz, runGraphvizCanvas', toLabel)
import Data.GraphViz.Attributes (crow, dashed, diamond, style)
import Data.GraphViz.Attributes.Complete (
  Attribute (ArrowHead, ArrowTail, Shape, Weight),
  Shape (DoubleOctagon, InvTrapezium))

data TransactionTypeDiagram = TransactionTypeDiagram {
  transactionName :: Text,
  scriptInputs :: Map Text InputFromScript,
  scriptOutputs :: Map Text OutputToScript,
  walletInputs :: Map Text Wallet,
  walletOutputs :: Map Text Wallet,
  mints :: Map Text MintOrBurn}

data InputFromScript = InputFromScript {
  fromScript :: Text,
  redeemer :: Maybe Text,
  datum :: Text,
  currencies :: [Currency]}

data OutputToScript = OutputToScript {
  toScript :: Text,
  datum :: Text,
  currencies :: [Currency]}

data MintOrBurn = MintOrBurn {
  mintingPolicy :: Text,
  redeemer :: Text,
  effects :: [MintQuantity Currency]}

data Wallet = Wallet {
  walletName :: Text,
  currencies :: [Currency]}

newtype Currency = Currency {currencyName :: Text} deriving (Eq, Ord, IsString, Show)

data NodeType =
  Transaction
  | TransactionInputFromScript
  | TransactionOutputToScript
  | TransactionInputFromWallet
  | TransactionOutputToWallet
  | MintingPolicy
  | ScriptAddress
  | WalletAddress
  deriving (Bounded, Enum, Eq, Ord, Show)

nodeType :: Int -> NodeType
nodeType = toEnum . (`mod` nodeTypeRange)

nodeTypeRange :: Int
nodeTypeRange = fromEnum (maxBound :: NodeType) + 1

isTransaction :: Int -> Bool
isTransaction = (== Transaction) . nodeType

isScriptUTxO :: Int -> Bool
isScriptUTxO = (`elem` [TransactionInputFromScript, TransactionOutputToScript]) . nodeType

transactionGraphToDot :: Text -> Gr NodeId Text -> DotGraph Int
transactionGraphToDot caption g = graphToDot params g' where
  g' :: Gr Text Text
  g' = first nodeLabel
       $ foldr dropNode g (filter ((> MintingPolicy) . nodeType) $ map fst $ labNodes g)
  -- drop script and wallet nodes
  dropNode :: Int -> Gr a b -> Gr a b
  dropNode n gr = snd (match n gr)
  params :: GraphvizParams Int Text Text Int Text
  params = defaultParams{
    globalAttributes = [GraphAttrs [toLabel caption]],
    clusterID = Num . Int,
    clusterBy = clustering,
    fmtCluster = \ n -> case lab g n of
      Just node -> [GraphAttrs [toLabel $ nodeLabel node]],
    fmtNode = \ (n, l) -> toLabel l : case nodeType n of
        Transaction{} -> [Shape DoubleOctagon]
        MintingPolicy{} -> [Shape InvTrapezium]
        _ -> [],
    -- give wallet inputs weight 0 to move wallet clusters below transaction
    fmtEdge = \ (src, dest, l) ->
      toLabel l
      : if isTransaction dest && nodeType src == TransactionInputFromWallet then [Weight $ Int 0]
        else if isTransaction dest && isScriptUTxO src && not ('@' `Text.elem` l) then [style dashed]
        else if isTransaction src && nodeType dest == MintingPolicy
             then if "@ mint or burn" `Text.isInfixOf` l then [ArrowHead diamond]
             else if "@ mint" `Text.isInfixOf` l then [ArrowHead crow]
             else []
        else []}
  clustering :: (Int, Text) -> NodeCluster Int (Int, Text)
  clustering (n, name)
    | (Just (ins, node, _, _outs), _) <- match n g,
      let noCluster = N (n, name) =
      case (nodeType n, nub $ filter (not . isTransaction . snd) ins) of
        (TransactionInputFromScript, [(_, address)]) -> C address noCluster
        (TransactionOutputToScript, [(_, address)]) -> C address noCluster
        (TransactionInputFromWallet, [(_, address)]) -> C address noCluster
        (TransactionOutputToWallet, [(_, address)]) -> C address noCluster
        (ScriptAddress, _) -> C n noCluster
        (WalletAddress, _) -> C n noCluster
        (MintingPolicy, _) -> noCluster
        (Transaction, _) -> noCluster

data OverlayMode = Distinct | Parallel | Serial deriving (Eq, Ord, Show)

data NodeId = ValidatorScriptNamed Text
            | WalletNamed Text
            | MintingPolicyNamed Text
            | TransactionNamed Text
            | ScriptUTxO {script :: Text, datum :: Text, currencies :: [Currency]}
            | WalletUTxO Text [Currency]
            deriving (Eq, Ord, Show)

nodeLabel :: NodeId -> Text
nodeLabel (MintingPolicyNamed name) = name
nodeLabel (ValidatorScriptNamed name) = name
nodeLabel (WalletNamed name) = name
nodeLabel (TransactionNamed name) = name
nodeLabel ScriptUTxO {datum, currencies} = currenciesLabel currencies <> "\n<" <> datum <> ">"
nodeLabel (WalletUTxO _ currencies) = currenciesLabel currencies

currenciesLabel :: [Currency] -> Text
currenciesLabel = Text.intercalate ", " . map currencyName

mintsLabel :: [MintQuantity Currency] -> Text
mintsLabel = Text.intercalate ", " . map describe where
  describe (Burn n c) = Text.pack ("burn " <> shows n " ") <> currencyName c
  describe (Mint n c) = Text.pack ("mint " <> shows n " ") <> currencyName c
  describe (BurnSome c) = Text.pack ("burn ") <> currencyName c
  describe (MintSome c) = Text.pack ("mint ") <> currencyName c
  describe (MintOrBurnSome c) = Text.pack ("mint or burn ") <> currencyName c

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
          Parallel -> foldMap nodeMapOfType [TransactionInputFromScript .. WalletAddress]
          _ -> foldMap nodeMapOfType [MintingPolicy, ScriptAddress, WalletAddress]
        nodeMapOfType :: NodeType -> Map NodeId Int
        nodeMapOfType n =
          Map.fromList (zip (nodesOfType n g) $ (nodeTypeRange*total+) <$> [fromEnum n, fromEnum n + nodeTypeRange ..])
        nodesOfType :: NodeType -> Gr NodeId Text -> [NodeId]
        nodesOfType t g = snd <$> filter ((t ==) . nodeType . fst) (labNodes g)
        nodeMap :: IntMap Int
        nodeMap = case mode of
          Serial -> IntMap.fromList $ mapMaybe targetUTxO $ labNodes g
          _ -> IntMap.fromList $ mapMaybe targetNode $ labNodes g
        targetNode :: (Int, NodeId) -> Maybe (Int, Int)
        targetNode (n, node) = (,) n <$> Map.lookup node nodeIdMap
        targetUTxO :: (Int, NodeId) -> Maybe (Int, Int)
        targetUTxO (n, node)
          | nodeType n `elem` [TransactionInputFromScript, TransactionInputFromWallet], -- input UTxOs
            (Just ([(_, address)], _, _, [(_, trans)]), _) <- match n g', -- address and transaction consuming the UTxO
            (Just ([], _, _, utxos), _) <- match address g', -- all UTxOs at the address
            (_, n'):_ <- reverse
                         $ filter (matchingOutput n node address trans . snd)
                         $ takeWhile ((< n) . snd) $ sortOn snd utxos
          = Just (n, n')
          | nodeType n `elem` [MintingPolicy, ScriptAddress, WalletAddress] = (,) n <$> Map.lookup node nodeIdMap
          | otherwise = Nothing
        -- | given an input UTxO, its address, transaction consuming it, and another UTxO,
        -- decide if the latter UTxO can serve as the former
        matchingOutput :: Int -> NodeId -> Int -> Int -> Int -> Bool
        matchingOutput n inputNode address trans n'
          | nodeType n' `elem` [TransactionOutputToScript, TransactionOutputToWallet],
            (Just (ins, _, outputNode, []), _) <- match n' g',
            [(_, trans')] <- filter ((< address) . snd) ins
          = inputNode == outputNode && trans' < trans
          | otherwise = False

gconcat :: [Gr a b] -> Gr a b
gconcat = uncurry mkGraph . mconcat . map (labNodes &&& labEdges)

transactionTypeGraph :: Int -> TransactionTypeDiagram -> Gr NodeId Text
transactionTypeGraph
  startIndex
  TransactionTypeDiagram{transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs, mints}
  = mkGraph nodes (nub edges) where
  nodes =
    (transactionNode, TransactionNamed transactionName)
    : (isNodes <> osNodes <> iwNodes <> owNodes <> mpNodes <> scriptNodes <> walletNodes)
  edges = concat [
    zipWith
      (\(name, InputFromScript{redeemer}) (n, _) -> (n, transactionNode, foldMap (<> "@") redeemer <> name))
      (Map.toList scriptInputs)
      isNodes,
    [ (n, transactionNode, name)
    | (name, Wallet w currencies) <- Map.toList walletInputs,
      let [(n, _)] = filter ((WalletUTxO w currencies ==) . snd) iwNodes],
    zipWith (\(name, OutputToScript{}) (n, _) -> (transactionNode, n, name)) (Map.toList scriptOutputs) osNodes,
    [ (transactionNode, n, name)
    | (name, Wallet w currencies) <- Map.toList walletOutputs,
      let [(n, _)] = filter ((WalletUTxO w currencies ==) . snd) owNodes],
    [ (transactionNode, n, redeemer <> " @ " <> mintsLabel effects)
    | (name, MintOrBurn mp redeemer effects) <- Map.toList mints,
      let [(n, _)] = filter ((MintingPolicyNamed mp ==) . snd) mpNodes],
    [ (scriptNode, n, "")
    | (n, ScriptUTxO name _ _) <- isNodes <> osNodes,
      let [(scriptNode, _)] = filter ((ValidatorScriptNamed name ==) . snd) scriptNodes],
    [ (walletNode, n, "")
    | (n, WalletUTxO name _) <- iwNodes <> owNodes,
      let [(walletNode, _)] = filter ((WalletNamed name ==) . snd) walletNodes]]
  transactionNode = nodeTypeRange*startIndex
  isNodes =
    [ (nodeTypeRange*n+1, ScriptUTxO fromScript datum currencies)
    | (n, (name, InputFromScript {fromScript, datum, currencies})) <- zip [startIndex..] $ Map.toList scriptInputs]
  osNodes =
    [ (nodeTypeRange*n+2, ScriptUTxO toScript datum currencies)
    | (n, (name, OutputToScript {toScript, datum, currencies})) <- zip [startIndex..] $ Map.toList scriptOutputs]
  iwNodes =
    [ (nodeTypeRange*n+3, WalletUTxO walletName currencies)
    | (n, (name, Wallet {walletName, currencies})) <- zip [startIndex..] $ Map.toList walletInputs]
  owNodes =
    [ (nodeTypeRange*n+4, WalletUTxO walletName currencies)
    | (n, (name, Wallet {walletName, currencies})) <- zip [startIndex..] $ Map.toList walletOutputs]
  mpNodes =
    [ (nodeTypeRange*n+5, MintingPolicyNamed name)
    | (n, name) <- zip [startIndex..] $ nub $ mintingPolicy . snd <$> Map.toList mints ]
  scriptNodes =
    [ (nodeTypeRange*n+6, ValidatorScriptNamed name)
    | (n, name) <- zip [startIndex..] $ nub $ map snd
      $ Map.toList ((fromScript <$> scriptInputs) <> (toScript <$> scriptOutputs)) ]
  walletNodes =
    [ (nodeTypeRange*n+7, WalletNamed name)
    | (n, name) <- zip [startIndex..] $ nub $ map snd $ Map.toList (walletName <$> walletInputs <> walletOutputs) ]