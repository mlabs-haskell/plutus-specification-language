{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Family.Diagram (
  TransactionGraph,
  OverlayMode (Distinct, Parallel, Serial),
  TransactionTypeDiagram (TransactionTypeDiagram,
                          transactionName,
                          scriptInputs,
                          scriptOutputs,
                          walletInputs,
                          walletOutputs,
                          mints),
  InputFromScript (InputFromScript,
                   fromScript,
                   redeemer,
                   datum,
                   currencies),
  OutputToScript (OutputToScript,
                  toScript,
                  datum,
                  currencies),
  MintOrBurn (MintOrBurn,
              mintingPolicy,
              redeemer,
              effects),
  Wallet (Wallet,
          walletName,
          datum,
          currencies),
  Currency (Currency),
  transactionTypeGraph,
  combineTransactionGraphs,
  transactionGraphToDot) where

import qualified Data.Char as Char
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.Graph.Inductive (Context, Gr, empty, gmap, lab, labEdges, labNodes, match, mkGraph, nodeRange)
import Data.GraphViz
  ( DotGraph,
    GlobalAttributes (GraphAttrs),
    GraphID (Num),
    GraphvizCanvas (Xlib),
    GraphvizOutput (Canon, DotOutput),
    GraphvizParams (clusterBy, clusterID, fmtCluster, fmtEdge, fmtNode, globalAttributes, isDotCluster),
    NodeCluster (C, N),
    Number (Int),
    defaultParams,
    graphToDot,
    preview,
    runGraphviz,
    runGraphvizCanvas',
    toLabel,
  )
import Data.GraphViz.Attributes (DirType (Back, NoDir), dashed, edgeEnds, normal, style)
import Data.GraphViz.Attributes.Complete
  ( Attribute (ArrowHead, ArrowTail, Shape, Weight),
    Shape (DoubleOctagon, InvTrapezium),
  )
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.List (foldl', nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Family (MintQuantity (Burn, BurnSome, Mint, MintOrBurnSome, MintSome), Transaction)

-- | Composable graph of a single or multiple transactions.
newtype TransactionGraph = TransactionGraph {getTransactionGrah :: Gr NodeId Text}

-- | Intermediate data structure to describe a 'Transaction' instance, before it's turned into a 'TransactionGraph'.
data TransactionTypeDiagram = TransactionTypeDiagram
  { transactionName :: Text,
    scriptInputs :: Map Text InputFromScript,
    scriptOutputs :: Map Text OutputToScript,
    walletInputs :: Map Text Wallet,
    walletOutputs :: Map Text Wallet,
    mints :: Map Text MintOrBurn
  }
  deriving (Eq, Show)

-- | A field of 'Transaction' 'Inputs' that's coming from a script
data InputFromScript = InputFromScript
  { fromScript :: Text,
    redeemer :: Maybe Text,
    datum :: Text,
    currencies :: [Currency]
  }
  deriving (Eq, Show)

-- | A field of 'Transaction' 'Outputs' that's going to a script
data OutputToScript = OutputToScript
  { toScript :: Text,
    datum :: Text,
    currencies :: [Currency]
  }
  deriving (Eq, Show)

-- | A field of 'Transaction' 'Mints'
data MintOrBurn = MintOrBurn
  { mintingPolicy :: Text,
    redeemer :: Text,
    effects :: [MintQuantity Text Currency]
  }
  deriving (Eq, Show)

-- | A field of 'Transaction' 'Inputs' or 'Outputs' that refers to a wallet UTxO
data Wallet = Wallet
  { walletName :: Text,
    datum :: Maybe Text,
    currencies :: [Currency]
  }
  deriving (Eq, Show)

newtype Currency = Currency {currencyName :: Text} deriving (Eq, Ord, IsString, Show)

data NodeType
  = Transaction
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

-- | Render a 'TransactionGraph' to Graphviz 'DotGraph' with the given caption.
transactionGraphToDot :: Text -> TransactionGraph -> DotGraph Int
transactionGraphToDot caption (TransactionGraph g) = graphToDot params g'
  where
    g' :: Gr Text Text
    g' =
      first nodeLabel $
        foldr dropNode g (filter ((> MintingPolicy) . nodeType) $ map fst $ labNodes g)
    -- drop script and wallet nodes
    dropNode :: Int -> Gr a b -> Gr a b
    dropNode n gr = snd (match n gr)
    params :: GraphvizParams Int Text Text Int Text
    params =
      defaultParams
        { globalAttributes = [GraphAttrs [toLabel caption]],
          clusterID = Num . Int,
          clusterBy = clustering,
          fmtCluster = \n -> case lab g n of
            Just node -> [GraphAttrs [toLabel $ nodeLabel node]],
          fmtNode = \(n, l) ->
            toLabel l : case nodeType n of
              Transaction {} -> [Shape DoubleOctagon]
              MintingPolicy {} -> [Shape InvTrapezium]
              _ -> [],
          -- give wallet inputs weight 0 to move wallet clusters below transaction
          fmtEdge = \(src, dest, l) ->
            toLabel l
              : if isTransaction dest && nodeType src == TransactionInputFromWallet
                then [Weight $ Int 0]
                else
                  if isTransaction dest && isScriptUTxO src && not ('@' `Text.elem` l)
                    then [style dashed]
                    else
                      if isTransaction src && nodeType dest == MintingPolicy
                        then
                          if "@ mint or burn" `Text.isInfixOf` l
                            then [edgeEnds NoDir]
                            else
                              if "@ mint" `Text.isInfixOf` l
                                then [edgeEnds Back]
                                else []
                        else []
        }
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

data OverlayMode = Distinct -- ^ the subgraphs won't share any UTxO nodes when combined 
                 | Parallel -- ^ the subgraphs will share all compatible UTxO nodes when combined
                 | Serial   -- ^ a later transaction's input may reuse an older transaction's output if compatible
                 deriving (Eq, Ord, Show)

data NodeId
  = ValidatorScriptNamed Text
  | WalletNamed Text
  | MintingPolicyNamed Text
  | TransactionNamed Text
  | ScriptUTxO {script :: Text, datum :: Text, currencies :: [Currency]}
  | WalletUTxO Text (Maybe Text) [Currency]
  deriving (Eq, Ord, Show)

nodeLabel :: NodeId -> Text
nodeLabel (MintingPolicyNamed name) = name
nodeLabel (ValidatorScriptNamed name) = name
nodeLabel (WalletNamed name) = name
nodeLabel (TransactionNamed name) = name
nodeLabel ScriptUTxO {datum, currencies} = currenciesLabel currencies <> "\n<" <> datum <> ">"
nodeLabel (WalletUTxO _ Nothing currencies) = currenciesLabel currencies
nodeLabel (WalletUTxO _ (Just datum) currencies) = currenciesLabel currencies <> "\n<" <> datum <> ">"

currenciesLabel :: [Currency] -> Text
currenciesLabel = Text.unlines . map currencyName

mintsLabel :: [MintQuantity Text Currency] -> Text
mintsLabel = Text.intercalate ", " . map describe
  where
    describe (Burn n c) = "burn " <> n <> " " <> currencyName c
    describe (Mint n c) = "mint " <> n <> " " <> currencyName c
    describe (BurnSome c) = "burn " <> currencyName c
    describe (MintSome c) = "mint " <> currencyName c
    describe (MintOrBurnSome c) = "mint or burn " <> currencyName c

-- | Combine multiple transaction graphs into a single one.
combineTransactionGraphs :: OverlayMode -> [TransactionGraph] -> TransactionGraph
combineTransactionGraphs mode gs = replaceFixedNodes mode totalNodeCount (TransactionGraph graphUnion)
  where graphUnion = gconcat $ getTransactionGrah <$> gs
        totalNodeCount = snd (nodeRange graphUnion) `div` nodeTypeRange + 1

-- | Unify the shareable nodes in a transaction graph.
replaceFixedNodes :: OverlayMode -> Int -> TransactionGraph -> TransactionGraph
replaceFixedNodes mode total (TransactionGraph g) =
  TransactionGraph $ mkGraph (sortOn (Down . fst) $ first switchNode <$> labNodes g) (switchEnds <$> labEdges g)
  where
    TransactionGraph g' = replaceFixedNodes Distinct total (TransactionGraph g)
    switchEnds (start, end, label) = (switchNode start, switchNode end, label)
    switchNode n = IntMap.findWithDefault n n nodeMap
    -- | the initial node mapping, shifting to fresh IDs all scripts and wallets across all transactions, and for
    -- 'Parallel' mode the UTxOs as well
    nodeIdMap :: Map NodeId Int
    nodeIdMap = foldMap nodeIdMapOfType $ case mode of
      Parallel -> [TransactionInputFromScript .. WalletAddress]
      _ -> [MintingPolicy, ScriptAddress, WalletAddress]
    nodeIdMapOfType :: NodeType -> Map NodeId Int
    nodeIdMapOfType n =
      Map.fromList (zip (nodesOfType n g) $ (nodeTypeRange * total +) <$> [fromEnum n, fromEnum n + nodeTypeRange ..])
    nodesOfType :: NodeType -> Gr NodeId Text -> [NodeId]
    nodesOfType t g = snd <$> filter ((t ==) . nodeType . fst) (labNodes g)
    -- | the final node mapping, unifying the node IDs according to the given 'OverlayMode'
    nodeMap :: IntMap Int
    nodeMap = IntMap.fromList $ mapMaybe targetNode $ labNodes g
    targetNode (n, node)
      | nodeType n `elem` [TransactionInputFromScript, TransactionInputFromWallet], -- input UTxOs
        mode == Serial,
        (Just ((_, address):_, _, _, (_, trans):_), _) <- match n g', -- address and transaction consuming the UTxO
        (Just ([], _, _, utxos), _) <- match address g', -- all UTxOs at the address
        (_, n') : _ <-
          filter (matchingOutput n node address trans . snd) $
            dropWhile ((>= n) . snd) $
              sortOn (Down . snd) utxos
      = Just (n, n')
      | otherwise = (,) n <$> Map.lookup node nodeIdMap
    -- Given an input UTxO, its address, transaction consuming it, and another UTxO, decide if the latter UTxO can
    -- serve as the former.
    matchingOutput :: Int -> NodeId -> Int -> Int -> Int -> Bool
    matchingOutput n inputNode address trans n'
      | nodeType n' `elem` [TransactionOutputToScript, TransactionOutputToWallet],
        (Just (ins, _, outputNode, []), _) <- match n' g'
      = any ((< trans) . snd) ins && outputNode `compatibleWith` inputNode
      | otherwise = False
    compatibleWith :: NodeId -> NodeId -> Bool
    compatibleWith (ScriptUTxO s1 d1 cs1) (ScriptUTxO s2 d2 cs2) = s1 == s2 && d1 == d2 && cs1 `quantitiesImply` cs2
    compatibleWith (WalletUTxO u1 d1 cs1) (WalletUTxO u2 d2 cs2) = u1 == u2 && d1 == d2 && cs1 `quantitiesImply` cs2
    compatibleWith n1 n2 = n1 == n2
    [] `quantitiesImply` [] = True
    _ `quantitiesImply` [Currency "AnythingElse"] = True
    (Currency c1 : cs1) `quantitiesImply` (Currency c2 : cs2) = c1 `quantityImplies` c2 && cs1 `quantitiesImply` cs2
    _ `quantitiesImply` _ = False
    c1 `quantityImplies` c2@"MinimumRequiredAda" = c1 == c2 || "RequiredAdaPlus " `Text.isPrefixOf` c1
    c1 `quantityImplies` c2
      | c1 == c2 = True
      | Just c2' <- Text.stripPrefix "Some " c2,
        Just c1' <- (Text.stripPrefix "Exactly " c1
                     <|> Text.stripPrefix "AtLeast " c1
                     <|> Text.stripPrefix "AtMost " c1)
                    >>= Text.stripPrefix " " . Text.dropWhile Char.isDigit =
          c1' == c2'
      | otherwise = False

gconcat :: [Gr a b] -> Gr a b
gconcat gs = mkGraph allNodes allEdges where
  (_, allNodes, allEdges) = foldl' addGraph (0, [], []) gs
  addGraph :: (Int, [(Int, a)], [(Int, Int, b)]) -> Gr a b -> (Int, [(Int, a)], [(Int, Int, b)])
  addGraph (maxNodeId, nodes, edges) g =
    (maxNodeId + nodeTypeRange * (snd (nodeRange g) `div` nodeTypeRange + 1),
     map (first (maxNodeId +)) (labNodes g) ++ nodes,
     map (firstTwo (maxNodeId +)) (labEdges g) ++ edges)
  firstTwo f (a, b, c) = (f a, f b, c)

transactionTypeGraph :: TransactionTypeDiagram -> TransactionGraph
transactionTypeGraph
  TransactionTypeDiagram {transactionName, scriptInputs, scriptOutputs, walletInputs, walletOutputs, mints} =
    TransactionGraph (mkGraph nodes $ nub edges)
    where
      nodes =
        (transactionNode, TransactionNamed transactionName)
          : (isNodes <> osNodes <> iwNodes <> owNodes <> mpNodes <> scriptNodes <> walletNodes)
      edges =
        concat
          [ zipWith
              (\(name, InputFromScript {redeemer}) (n, _) -> (n, transactionNode, foldMap (<> "@") redeemer <> name))
              (Map.toList scriptInputs)
              isNodes,
            [ (n, transactionNode, name)
              | (name, Wallet w d currencies) <- Map.toList walletInputs,
                let [(n, _)] = filter ((WalletUTxO w d currencies ==) . snd) iwNodes
            ],
            zipWith (\(name, OutputToScript {}) (n, _) -> (transactionNode, n, name)) (Map.toList scriptOutputs) osNodes,
            [ (transactionNode, n, name)
              | (name, Wallet w d currencies) <- Map.toList walletOutputs,
                let [(n, _)] = filter ((WalletUTxO w d currencies ==) . snd) owNodes
            ],
            [ (transactionNode, n, redeemer <> " @ " <> mintsLabel effects)
              | (name, MintOrBurn mp redeemer effects) <- Map.toList mints,
                let [(n, _)] = filter ((MintingPolicyNamed mp ==) . snd) mpNodes
            ],
            [ (scriptNode, n, "")
              | (n, ScriptUTxO name _ _) <- isNodes <> osNodes,
                let [(scriptNode, _)] = filter ((ValidatorScriptNamed name ==) . snd) scriptNodes
            ],
            [ (walletNode, n, "")
              | (n, WalletUTxO name _ _) <- iwNodes <> owNodes,
                let [(walletNode, _)] = filter ((WalletNamed name ==) . snd) walletNodes
            ]
          ]
      transactionNode = fromEnum Transaction
      isNodes =
        [ (nodeTypeRange * n + fromEnum TransactionInputFromScript, ScriptUTxO fromScript datum currencies)
          | (n, (name, InputFromScript {fromScript, datum, currencies})) <- zip [0 ..] $ Map.toList scriptInputs
        ]
      osNodes =
        [ (nodeTypeRange * n + fromEnum TransactionOutputToScript, ScriptUTxO toScript datum currencies)
          | (n, (name, OutputToScript {toScript, datum, currencies})) <- zip [0 ..] $ Map.toList scriptOutputs
        ]
      iwNodes =
        [ (nodeTypeRange * n + fromEnum TransactionInputFromWallet, WalletUTxO walletName datum currencies)
          | (n, (name, Wallet {walletName, datum, currencies})) <- zip [0 ..] $ Map.toList walletInputs
        ]
      owNodes =
        [ (nodeTypeRange * n + fromEnum TransactionOutputToWallet, WalletUTxO walletName datum currencies)
          | (n, (name, Wallet {walletName, datum, currencies})) <- zip [0 ..] $ Map.toList walletOutputs
        ]
      mpNodes =
        [ (nodeTypeRange * n + fromEnum MintingPolicy, MintingPolicyNamed name)
          | (n, name) <- zip [0 ..] $ nub $ mintingPolicy . snd <$> Map.toList mints
        ]
      scriptNodes =
        [ (nodeTypeRange * n + fromEnum ScriptAddress, ValidatorScriptNamed name)
          | (n, name) <-
              zip [0 ..] $
                nub $
                  map snd $
                    Map.toList (fromScript <$> scriptInputs) <> Map.toList (toScript <$> scriptOutputs)
        ]
      walletNodes =
        [ (nodeTypeRange * n + fromEnum WalletAddress, WalletNamed name)
          | (n, name) <- zip [0 ..] $ nub $
              walletName . snd <$> Map.toList walletInputs <> Map.toList walletOutputs
        ]
