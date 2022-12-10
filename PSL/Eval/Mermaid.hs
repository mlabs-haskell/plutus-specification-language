module PSL.Eval.Mermaid (mermaidDiagram) where

import Control.Monad (unless, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import Data.Foldable (for_, traverse_)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import PSL (PDiagram)
import PSL.Eval.Backend
import PSL.Eval.Interval qualified as Iv
import PSL.Eval.Tx
import Plutarch.Core
import Prettyprinter (Pretty (pretty))

type Decl = StateT Int (Writer Text)

newtype Name = Name Text
  deriving stock (Eq, Ord)

data Orientation
  = TopBottom
  | BottomTop
  | LeftRight
  | RightLeft

runMermaid :: Decl a -> Text
runMermaid m = execWriter (evalStateT m 0)

out :: [Text] -> Decl ()
out x = lift $ tell (T.unwords x <> "\n")

newName :: Decl Name
newName = state \x -> (Name $ "node" <> T.pack (show x), x + 1)

orientation :: Orientation -> Text
orientation = \case
  TopBottom -> "TB"
  BottomTop -> "BT"
  LeftRight -> "LR"
  RightLeft -> "RL"

flowchart :: Orientation -> Decl a -> Decl a
flowchart dir decls = do
  out ["flowchart", orientation dir]
  decls

data NodeShape
  = NRegular
  | NRounded
  | NProcedure
  | NStadium
  | NAsymmetric
  | NHexagon

nodeShape :: NodeShape -> (Text, Text)
nodeShape = \case
  NRegular -> ("[", "]")
  NRounded -> ("(", ")")
  NProcedure -> ("[[", "]]")
  NStadium -> ("((", "))")
  NAsymmetric -> (">", "]")
  NHexagon -> ("{{", "}}")

escape :: Text -> Text
escape = T.replace "\n" "\\n" . T.replace "\"" "&quot;"

subgraphFrom :: Orientation -> Name -> Text -> Decl a -> Decl a
subgraphFrom dir (Name nm) txt decl = do
  out ["subgraph", nm <> "[\"" <> escape txt <> "\"]"]
  out ["direction", orientation dir]
  x <- decl
  out ["end"]
  pure x

subgraph :: Orientation -> Text -> Decl a -> Decl Name
subgraph dir txt decl = do
  nm <- newName
  subgraphFrom dir nm txt decl
  pure nm

nodeFrom :: NodeShape -> Name -> Text -> Decl ()
nodeFrom shape (Name nm) txt = do
  let (sl, sr) = nodeShape shape
  out [nm <> sl <> "\"" <> escape txt <> "\"" <> sr]

node :: NodeShape -> Text -> Decl Name
node shape txt = do
  nm <- newName
  nodeFrom shape nm txt
  pure nm

link' :: Maybe Text -> Int -> Name -> Name -> Decl ()
link' txt len (Name from) (Name to) =
  out $ [from, T.replicate len "-" <> "-->"] <> fmap (\x -> "|\"" <> escape x <> "\"|") (maybeToList txt) <> [to]

link, linkLong :: Name -> Name -> Decl ()
link = link' Nothing 0
linkLong = link' Nothing 1

pshow :: Pretty a => a -> Text
pshow x = docToText $ pretty x

renderDiagram :: Diagram -> Decl ()
renderDiagram diag@(Diagram ownIns ins inMap outs _ _ _) =
  flowchart LeftRight do
    txName <- txDiagram diag
    for_ ownIns \utxo -> do
      utxoName <- ownUTXODiagram utxo
      linkLong utxoName txName
    inRefNames <- for (Set.toList ins) \utxo -> do
      utxoName <- node NAsymmetric $ pshow utxo
      link utxoName txName
      pure utxoName
    let inRefToNames = Map.fromList $ zip (Set.toList ins) inRefNames
    for_ inMap \(ref, utxo) -> do
      name <- utxoDiagram "" utxo
      traverse_ (link name) $ Map.lookup ref inRefToNames
    for_ outs \utxo -> do
      utxoName <- outputDiagram utxo
      link txName utxoName

outputDiagram :: Output -> Decl Name
outputDiagram (OutputWitness utxo) = utxoDiagram "Witness: " utxo
outputDiagram (OutputCreate utxo) = utxoDiagram "Create: " utxo
outputDiagram (OutputOwn utxo) = ownUTXODiagram utxo

txDiagram :: Diagram -> Decl Name
txDiagram (Diagram _ _ _ _ sign mint tr) = subgraph LeftRight "Transaction" do
  unless (null sign) $ void $ subgraph TopBottom "Signatories" do
    for_ sign (node NAsymmetric . pshow)
  unless (mint == mempty) $
    void $
      subgraph LeftRight "Mint" $
        valueDiagram mint
  unless (tr == Iv.always) $ void $ node NRegular $ "ValidRange: " <> pshow tr

ownUTXODiagram :: OwnUTXO -> Decl Name
ownUTXODiagram (OwnUTXO val dat) = subgraph LeftRight "Own" do
  node NHexagon $ docToText $ pretty dat
  valueDiagram val

utxoDiagram :: Text -> UTXO -> Decl Name
utxoDiagram prefix (UTXOAddr addr val dat) = subgraph LeftRight (prefix <> "Address " <> pshow addr) do
  node NHexagon $ docToText $ pretty dat
  valueDiagram val
utxoDiagram prefix (UTXOProtocol name val dat) = subgraph LeftRight (prefix <> "Protocol " <> pshow name) do
  node NHexagon $ docToText $ pretty dat
  valueDiagram val

valueDiagram :: Value -> Decl ()
valueDiagram (Value ada own others) = do
  unless (ada == 0) $ void $ node NRounded $ "ADA: " <> pshow ada
  unless (Map.null own) $ void $ subgraph TopBottom "Own value" do
    for_ (Map.toList own) tokenDiagram
  for_ (Map.toList others) \(cs, subvalue) -> subgraph TopBottom (pshow cs) do
    for_ (Map.toList subvalue) tokenDiagram

tokenDiagram :: (TokenName, Integer) -> Decl Name
tokenDiagram (tok, n) = node NRounded $ pshow tok <> ": " <> pshow n

mermaidDiagram :: Term EK (PDiagram d) -> Text
mermaidDiagram (MkTerm d) = runMermaid $ renderDiagram $ intoDiagram (runIdentity d)
