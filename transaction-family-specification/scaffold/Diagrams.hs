{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (forkIO)
import Data.GraphViz
  ( GraphvizCanvas (Xlib),
    GraphvizOutput (Canon, DotOutput, Svg),
    runGraphviz,
    runGraphvizCanvas',
  )
import Family
import Family.Diagram
  ( OverlayMode (Distinct, Parallel, Serial),
    combineTransactionGraphs,
    transactionGraphToDot,
    transactionTypeGraph,
  )
import Family.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType)
import qualified Language.Haskell.TH as TH
-- import the transaction instances
import qualified Spec

-- transaction diagrams
diagram = $([t|'Spec.MyTransaction "Alice" 4|] >>= untypedDiagramForTransactionType)

-- rendering
main = do
  let graph = transactionTypeGraph diagram
      dot = transactionGraphToDot "transaction diagram" graph
  forkIO (runGraphvizCanvas' dot Xlib)
  runGraphviz dot Svg ("diagram.svg")
