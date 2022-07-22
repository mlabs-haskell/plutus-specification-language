# Typing transaction families as HKDs

~~~ {.haskell}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification,
             PolyKinds, StandaloneKindSignatures #-}
module Diagram where

import Families
import HKD
import Data.Kind (Type)
import Generics.Deriving.Base (Generic1)
import Data.Graph.Inductive (Gr, mkGraph)
--import Data.GraphViz.Types.Graph (mkGraph)

type W1 :: (k1 -> k2 -> Type) -> k1 -> Type
type W2 :: (k1 -> k2 -> Type) -> k2 -> Type
data W1 t a1 = forall a2. W1 (t a1 a2)
data W2 t a2 = forall a1. W2 (t a1 a2)

transactionTypeDiagram :: forall t. (Transaction t, Generic1 (W1 (Outputs t)), Generic1 (W2 (Outputs t)))
                       => Gr [Char] [Char]
transactionTypeDiagram = mkGraph nodes edges where
  nodes = (transactionNode, "transaction") : (inputNodes <> outputNodes)
  edges = [(-n, transactionNode, "input") | n <- inputNodes]
       <> [(transactionNode, n, "output") | n <- outputNodes]
  transactionNode = 0
  inputNodes = []
  outputNodes = [] -- hpure @(Outputs t)
~~~
