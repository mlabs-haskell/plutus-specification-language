module PSL

import Data.List
import Data.List.Elem

%default total

data ByteString : Type where

data Data : Type where
  DataConstr : Integer -> List Data -> Data
  DataMap : List (Data, Data) -> Data
  DataList : List Data -> Data
  DataInt : Integer -> Data
  DataBS : ByteString -> Data

data PubKeyHash : Type where

data Hash : Type where

data ScriptContext : Type where

data Script : Type where
  MkScript : (f : Data -> Data -> ScriptContext -> Bool) -> Script

scriptHash : Script -> Hash
scriptHash = ?scriptHashjjj

scriptEquality : (f : Script) -> (g : Script) -> (scriptHash f === scriptHash g) -> (f === g)
scriptEquality = ?scriptEqualityjj

data ScriptAddress : Type where

data Datum : Type where
  MkDatum : Data -> Datum

data CurrencySymbol : Type where

data TokenName : Type where

data Value : Type where
  MkValue : List (CurrencySymbol, (List (TokenName, Integer))) -> Value

Address = Either PubKeyHash ScriptAddress

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
  ParallelTx : TxCombination -> TxCombination -> TxCombination
  --ChoiceTx   : TxCombination -> TxCombination -> TxCombination
  PredicateTx : TxCombination -> Bool -> TxCombination

---- Marlowe in types ----

data MarloweContract : Type where
  Close : MarloweContract
  Let : ValueExpression -> (Value -> Contract) -> Contract
  If : Observation -> MarloweContract -> MarloweContract -> MarloweContract
  When : [(MarloweAction, MarloweContract)] -> Timeout -> MarloweContract -> MarloweContract
  Pay : Account -> Account -> Value -> MarloweContract -> MarloweContract

-- data Value

data TransactionInput : a -> Type where
  Choice : Account -> Integer -> TransactionInput Integer
  Deposit : Account -> Value -> TransactionInput Value
  Notify : Observation a -> TransactionInput a -- waits for the observation to be true

data ValueExpression

data Observation

data Account

data Timeout : Type where
  Timeout : Integer -> Timeout

---- end Marlowe ----

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

data ChessColor = Black | White

exampleScript : ScriptAddress
exampleScript = ?hy

hasChessBoard : Datum -> Type 
hasChessBoard (MkDatum jdawd) = ?hasChessBoardHole  -- something something data has field that is something serialised 

hasNextPlayer : Datum -> Type
hasNextPlayer = ?hasNextPlayerhole -- unsure what this is supposed to be 

validDatum : Datum -> Type
validDatum dat = (hasChessBoard dat, hasNextPlayer dat)

-- (<) : Integer -> Integer -> Bool

validExample : TxOut -> Type
validExample utxo = 
  DPair CurrencySymbol $ \cs =>
  DPair TokenName $ \tn =>
  DPair Integer $ \amount => 
  DPair Datum $ \datum =>
    ( validDatum datum 
    , utxo.datum === Just datum
    , (amount < 1_000_000_000) === True
    , utxo.address === Right exampleScript
    , utxo.value === MkValue [(cs, [(tn, amount)])]
    )

feeInput : TxOut
feeInput = ?xufhiawfh

example : TxOut -> TxOut -> TxOut -> TxInfo
example blackUtxo whiteUtxo chessUtxo = MkTxInfo
  { inputs = 
    [ chessUtxo
    , case ?chessColor chessUtxo of
        Black => blackUtxo
        White => whiteUtxo
    , feeInput
    ]
  , outputs =
    [ ?progressChessUtxo (?findChessAction chessUtxo) chessUtxo
    , case ?chessColor' chessUtxo of
        Black => blackUtxo
        White => whiteUtxo
    ] 
  , mint = ?mint
  , signatures = ?sigs
  }

exampleProof : 
     (blackUtxo : DPair TxOut ?validBlack) 
  -> (whiteUtxo : DPair TxOut ?validWhite)
  -> (chessUtxo : DPair TxOut PSL.validExample)
  ->
    let 
      tx = example (fst blackUtxo) (fst whiteUtxo) (fst chessUtxo)
    in
    ( cardanoValidate tx === True
    , Elem (fst whiteUtxo) tx.inputs)

--tchess : TxInfo -> Type
--tchess t = (utxo : TxOut) -> validChess utxo -> (t.inputs = Cons utxo Nil, t.outputs = Nil) -> cardanoValidate t = True

----------------------------------------------------------------------------
-- Specification of "there is a chain of transactions that let you withdraw"
--

chessUTXO : Type
chessUTXO = DPair TxOut validExample

withdraws : PSL.chessUTXO -> TxInfo -> Type
withdraws (MkDPair utxo _ ) tx = DPair PubKeyHash $ \ wallet =>
   ( Elem utxo tx.inputs
   , Elem (?paid wallet (MkValue ?utxoValue)) tx.outputs
   )

validates : TxInfo -> Type
validates utxo = cardanoValidate utxo === True

nextUTXO : PSL.chessUTXO -> (tx : TxInfo) -> PSL.validates tx -> PSL.chessUTXO -> Type
nextUTXO = ?nextutxojjj

data EventuallyWithdraws : PSL.chessUTXO -> Type where

  Step : (tx : TxInfo) -> (v : validates tx) -> (utxo' : PSL.chessUTXO)
         -> (utxo : PSL.chessUTXO) -> PSL.nextUTXO utxo tx v utxo'
         -> EventuallyWithdraws utxo'
         -> EventuallyWithdraws utxo
  Done : (tx : TxInfo) -> (v : validates tx)
         -> PSL.withdraws utxo tx
         -> EventuallyWithdraws utxo

-- :D

-------------------------------------------------------------------------------
-- Specification Stable-Coin
--
--
-- We have the concepts of vaults. If you put in collateral, you get out the
-- stablecoin. If you put in the stablecoin, you get out the collateral.
-- In the real world, the ratio between these are determined by an oracle.
-- For our use case, we assume that the ratio is fixed, such that liquidations are not
-- necessary.
--
-- At a UTXO level, we have 2 minting policies and 1 validator.
-- The minting policy Vm allows minting tokens iff those tokens are locked by the validator V.
-- The minting policy S allows minting tokens iff there is an input locked by the validator V.
-- The validator V only allows consumption iff:
-- - There is exactly one output with the token for Vm, where the output is locked by V again.
-- - The amount of S minted/burned is the difference in the amount of collateral locked by the input vault
--   and output vault, multiplied by the ratio r.
--
-- OLD:
--
-- The vault can hold collateral and mint currency when given collateral.
-- If you give the vault the currency back, you will get a corresponding amount of
-- collateral back. The currency will be burned.
-- There is a (fixed in this case) ratio between the collateral and the currency.
-- The global ratio between the collateral and currency that exist in the set of live UTXOs,
-- should be maintained.

-- assume ratioCollateral does not change (much?)
-- assume we have 1 vault
-- baseCase      : 
--   ratioCollateral * (0 collateral) === 0 currency -- True
-- inductiveCase : 
--   assume (prevCollateralValue + ratioCollateral * Y) Collateral === (prevCurrencyVal + Y) Currency
--   prove for (Y+1) 

data Ledger : Type where
  MkLedger : List (Valid TxOut) -> Ledger

-- we have a MP (called m) that produces a Token that is always locked with a specifc Script (called s) 

hasToken : TxOut -> Type
hasToken utxo = ?hasToken

isAtAddress : ScriptAddress -> TxOut -> Type
isAtAddress scr utxo = utxo.address === Right scr
 
all : (A -> Type) -> List A -> Type
all P xs = (x : A) -> Elem x xs -> P x

tokenStaysAtAddress : ScriptAddress -> TxInfo -> Type
tokenStaysAtAddress scr tx = 
   all (\ utxo => hasToken utxo -> isAtAddress scr utxo) (tx.inputs)
   -> all (\ utxo => hasToken utxo -> isAtAddress scr utxo) (tx.outputs)

