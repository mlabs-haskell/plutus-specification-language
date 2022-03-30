module PSL

import Data.List
import Data.DPair

%default total

public export
record Set (a : Type) where
  constructor MkSet
  indexType : Type
  contents : indexType -> a

public export
ProtocolName : Type -> Type

public export
SomeProtocolName : Type
SomeProtocolName = Exists ProtocolName

MkSomeProtocol : ProtocolName d -> SomeProtocolName
MkSomeProtocol p = Evidence p

protocolEquality : ProtocolName a -> ProtocolName b -> Bool

public export
UTXORef : Type

public export
TokenName : Type

public export
PubKeyHash : Type

public export
Map : Type -> Type -> Type

public export
NilMap : Map a b

public export
Value : Type
Value = Map SomeProtocolName (Map TokenName Integer)

public export
record UTXOFor (p : ProtocolName d) where -- UTXO *for a specific protocol*.
  constructor MkUTXOFor
  value : Value
  datum : d
  staker : Maybe SomeProtocolName -- will change the set of accepted txes as a free variable
  -- ^ I'll get back to that TODO

public export
UTXO : Type
UTXO = DPair SomeProtocolName (\p => UTXOFor (fst p))

public export
record TxIn where
  constructor MkTxIn
  utxo : UTXO
  ref : Maybe UTXORef -- Plutus type

public export
record TxOut (context : SomeProtocolName) where
  constructor MkTxOut
  utxo : UTXO
  unique : if context == fst utxo then () else Bool

public export
TimeRange : Type

public export
record TxDiagram (context : SomeProtocolName) where
  constructor MkTxDiagram
  inputs : List TxIn
  outputs : List (TxOut context)
  mint : Value
  signatures : List PubKeyHash
  validRange : TimeRange

-- Order is important
public export
data ListIn' : List a -> List a -> Type where
  MkListInNil : ListIn' Nil y
  MkListInCons : ListIn' xt yt -> ListIn' (x :: xt) (y :: yt)

public export
data ListIn : List a -> List a -> Type where
  MkListInPrefix : ListIn' x y' -> ListIn x (y ++ y')

public export
data ListFiltered : (a -> Type) -> List a -> List a -> Type where
  MkListFilteredNil : ListFiltered f Nil Nil
  MkListFilteredCons : {0 f : a -> Type} -> f h -> ListFiltered f xt yt -> ListFiltered f (h :: xt) (h :: yt)
  MkListFilteredSkip : {0 f : a -> Type} -> Not (f h) -> ListFiltered f xt y -> ListFiltered f (h :: xt) y

public export
lookupMap : a -> Map a b -> Maybe b

-- fixme
public export
SetSubset : List a -> List a -> Type

public export
ValueSubset : Value -> Value -> Type

public export
record Tx where
  constructor MkTx 
  inputs : List TxIn
  outputs : List UTXO
  mint : Value
  signatures : List PubKeyHash
  validRange : TimeRange

public export
TxMatches : {p : SomeProtocolName} -> Tx -> TxDiagram p -> Type
TxMatches tx d = 
  ( tx.validRange === d.validRange
  , ListIn d.inputs tx.inputs
  , ListIn (map (.utxo) d.outputs) tx.outputs
  , ValueSubset d.mint tx.mint
  , SetSubset d.signatures tx.signatures
  , DPair (List TxIn) $ \f => (ListFiltered (\u => fst u.utxo === p) tx.inputs f, ListIn f d.inputs)
  , DPair (List UTXO) $ \f => (ListFiltered (\u => fst u === p) tx.outputs f, ListIn f (map (.utxo) d.outputs))
  , DPair _ $ \f =>
    DPair _ $ \f' =>
    (lookupMap p tx.mint === f, lookupMap p d.mint === f', f === f')
  )

public export
record Protocol where
  constructor MkProtocol
  datumType : Type
  permissible' : (self : ProtocolName datumType) -> Set (TxDiagram self)

public export
nameFor : (p : Protocol) -> ProtocolName p.datumType

public export
permissible : (p : Protocol) -> Set (TxDiagram $ MkSomeProtocol $ nameFor p)
permissible p = p.permissible' (nameFor p)

public export
sameProtocol :
  (p1 : Protocol) ->
  (p2 : Protocol) ->
  (nameFor p1 === nameFor p2) ->
  p1 === p2

public export
ReducibleProtocol : (p : Protocol) -> (UTXOFor (nameFor p) -> Type) -> Type

public export
DisjointProtocol : Protocol -> Type
DisjointProtocol p =
  (x : (permissible p).indexType) ->
  (y : (permissible p).indexType) ->
  (tx : Tx) ->
  Not (x === y) ->
  Not (TxMatches tx ((permissible p).contents x), TxMatches tx ((permissible p).contents y))

public export
CoveredProtocol : Protocol -> Type

public export
record ProtocolSoundness (protocol : Protocol) where
  constructor MkProtocolSoundness
  reducibleType : UTXOFor (nameFor protocol) -> Type
  reducibility : ReducibleProtocol protocol reducibleType
  disjointness : DisjointProtocol protocol
  coverage : CoveredProtocol protocol

{-
Ledger : Type
Ledger = List UTXODaniele Procidaz

ApplyableTx : TxDiagram -> Ledger -> Type
ApplyableTx = ?applyableTxHole

applyTx : (ledger : Ledger) -> (tx : TxDiagram) -> (ApplyableTx tx ledger) -> Ledger
applyTx = ?applyTxHole

-- https://github.com/mlabs-haskell/plutus-specification-language/tree/master/new

-- https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week10.html

-}
