# Diagrams of transaction types

Once the transaction family is specified at the type level, we can import the `Family.Diagram` and `Family.DiagramTH`
modules in order to automatically generate various diagrams of our transactions in various combinations.

~~~ {.haskell}
{-# LANGUAGE DataKinds, DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}
module Diagram where

import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Family.Diagram (
  TransactionTypeDiagram,
  OverlayMode (Distinct, Parallel, Serial),
  transactionGraphToDot, transactionTypeGraph, transactionTypeFamilyGraph)
import Data.GraphViz (
  GraphvizCanvas (Xlib), GraphvizOutput (Canon, DotOutput),
  runGraphviz, runGraphvizCanvas')
import qualified Language.Haskell.TH as TH
import Family.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType)
import qualified HKD
~~~

First we generate some diagrams using Template Haskell splices from `Family.Diagram.TH`:

~~~ {.haskell}
update, exchange, drain :: TransactionTypeDiagram
update = $( [t| 'HKD.UpdateOracle 1 |] >>= untypedDiagramForTransactionType)
exchange = $( [t| 'HKD.Exchange 1 2 |] >>= untypedDiagramForTransactionType)
drain = $$(diagramForTransactionType $ TH.PromotedT $ TH.mkName "HKD.DrainCollectedFees")
~~~

Then we can convert the diagrams to GraphViz either on their own, or combining multiple transaction types in a single
graph. There are three different modes of combinations: `Distinct`, `Parallel`, and `Serial`. The serial mode appears
to be most intuitive.

~~~ {.haskell}
main = do
  let g1 = transactionGraphToDot "exchange" (transactionTypeGraph 0 exchange)
      g2 = transactionGraphToDot "distinct" (transactionTypeFamilyGraph Distinct [update, exchange, drain])
      g3 = transactionGraphToDot "parallel" (transactionTypeFamilyGraph Parallel [update, exchange, drain])
      g4 = transactionGraphToDot "serial" (transactionTypeFamilyGraph Serial [update, exchange, drain])
  forkIO (runGraphvizCanvas' g2 Xlib)
  forkIO (runGraphvizCanvas' g3 Xlib)
  forkIO (runGraphvizCanvas' g4 Xlib)
  runGraphviz g4 Canon "update-exchange-drain.dot"
~~~
