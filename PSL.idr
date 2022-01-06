module PSL

%default total

data ByteString : Type where

data Data : Type where
  DataConstr : Integer -> List Data -> Data
  DataMap : List (Data, Data) -> Data
  DataList : List Data -> Data
  DataInt : Integer -> Data
  DataBS : ByteString -> Data

data PubKeyHash : Type where

data ScriptAddress : Type where

data Datum : Type where
  MkDatum : Data -> Datum

data CurrencySymbol : Type where

data TokenName : Type where

data Value : Type where
  MkValue : List (CurrencySymbol, (List (TokenName, Integer))) -> Value

record TxOut where
  datum : Maybe Datum
  address : Either PubKeyHash ScriptAddress
  value : Value

record TxInfo where
  constructor MkTxInfo
  inputs : List TxOut
  outputs : List TxOut
  mint : Value
  signatures : List PubKeyHash

data TxCombination : Type where
  EmptyTx    : TxCombination
  PureTx     : TxInfo -> TxCombination 
  SerialTx   : TxCombination -> TxCombination -> TxCombination
  ReallySerialTx   : (x : TxCombination) -> (y : TxCombination) -> ((compile x).outputs <-> (compile y).inputs === (Nil,Nil)) -> TxCombination
  ParallelTx : TxCombination -> TxCombination -> TxCombination
  --ChoiceTx   : TxCombination -> TxCombination -> TxCombination
  PredicateTx : TxCombination -> Bool -> TxCombination

record TxEffect where
  constructor MkTxEffect
  inputs : List TxOut
  outputs : List TxOut

consume : List TxOut -> List TxOut -> (List TxOut, List TxOut)
consume _ _ = ?consume_hole

infixl 9 <-> 
(<->) : List TxOut -> List TxOut -> (List TxOut, List TxOut)
(<->) = consume

compile : TxCombination -> TxEffect
compile EmptyTx =
  MkTxEffect {
    inputs = Nil,
    outputs = Nil
  }

compile (PureTx t) =
  MkTxEffect {
    inputs = t.inputs,
    outputs = t.outputs
  }
compile (SerialTx x y) =
  let (outputs, inputs) = (compile x).outputs <-> (compile y).inputs in
  MkTxEffect {
    inputs = (compile x).inputs <+> inputs,
    outputs = outputs <+> (compile y).outputs
  }
compile (ReallySerialTx x y) =
  let (outputs, inputs) = (compile x).outputs <-> (compile y).inputs in
  MkTxEffect {
    inputs = (compile x).inputs <+> inputs,
    outputs = outputs <+> (compile y).outputs
  }
compile (ParallelTx x y) =
  MkTxEffect {
    inputs = (compile x).inputs <+> (compile y).inputs,
    outputs = (compile x).outputs <+> (compile y).outputs
  }
compile (PredicateTx x _) = compile x

cardanoValidate : TxInfo -> Bool
cardanoValidate = ?a

checkCombination : TxCombination -> Bool
checkCombination EmptyTx = True
checkCombination (PureTx x) = cardanoValidate x
checkCombination (SerialTx x y) = checkCombination x && checkCombination y
checkCombination (ParallelTx x y) = checkCombination x && checkCombination y
checkCombination (PredicateTx x p) = checkCombination x == p


-- Example
validChess : TxOut -> Type
validChess = ?vc

exampleScript : ScriptAddress
exampleScript = ?hy

validExample : TxOut -> Type
validExample utxo = 
  DPair CurrencySymbol $ 
    \cs => DPair TokenName $ 
      \tn => DPair Integer $ 
        \amount => ((amount < 1_000_000_000) === True, utxo.address === Right exampleScript, utxo.value === MkValue [(cs, [(tn, amount)])])

example : (utxo : DPair TxOut PSL.validExample) -> TxCombination
example = ?jx

allExample : (utxo : _) -> (PSL.checkCombination (PSL.example utxo) === True)
allExample = ?oj

--tchess : TxInfo -> Type
--tchess t = (utxo : TxOut) -> validChess utxo -> (t.inputs = Cons utxo Nil, t.outputs = Nil) -> cardanoValidate t = True
--

