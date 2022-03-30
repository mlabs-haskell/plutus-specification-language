module PSL

import Data.List
import Data.List.Elem

%default total

record Set (a : Type) where
  constructor MkSet
  b : Type
  f : b -> a

ProtocolName : Type

Eq ProtocolName where
  x == y = True

datumType : ProtocolName -> Type

UTXORef : Type

TokenName : Type

PubKeyHash : Type

Map : Type -> Type -> Type

Value = Map ProtocolName (Map TokenName Integer)

record UTXOFor (p : ProtocolName) where -- UTXO *for a specific protocol*.
  constructor MkUTXOFor
  value : Value
  datum : datumType p
  staker : ProtocolName -- will change the set of accepted txes as a free variable
  -- ^ I'll get back to that TODO

UTXO : Type
UTXO = DPair ProtocolName UTXOFor

record TxIn where
  constructor MkTxIn
  utxo : UTXO
  ref : Maybe UTXORef -- Plutus type

record TxOut (context : ProtocolName) where
  constructor MkTxOut
  utxo : UTXO
  unique : if context == fst utxo then () else Bool

TimeRange : Type

record TxDiagram (context : ProtocolName) where
  constructor MkTxDiagram
  inputs : List TxIn
  outputs : List (TxOut context)
  mint : Value
  signatures : List PubKeyHash
  validRange : TimeRange

-- Order is important
data ListIn' : List a -> List a -> Type where
  MkListInNil : ListIn' Nil y
  MkListInCons : ListIn' xt yt -> ListIn' (x :: xt) (y :: yt)

data ListIn : List a -> List a -> Type where
  MkListInPrefix : ListIn' x y' -> ListIn x (y ++ y')

data ListFiltered : (a -> Type) -> List a -> List a -> Type where
  MkListFilteredNil : ListFiltered f Nil Nil
  MkListFilteredCons : f h -> ListFiltered f xt yt -> ListFiltered f (h :: xt) (h :: yt)
  MkListFilteredSkip : Not (f h) -> ListFiltered f xt y -> ListFiltered f (h :: xt) y

lookup : a -> Map a b -> Maybe b

-- fixme
SetSubset : List a -> List a -> Type

ValueSubset : Value -> Value -> Type

record Tx where
  constructor MkTx 
  inputs : Set TxIn
  outputs : List UTXO
  mint : Value
  signatures : Set PubKeyHash
  validRange : TimeRange

TxMatches : {p : ProtocolName} -> Tx -> TxDiagram p -> Type
TxMatches tx d = 
  ( tx.validRange === d.validRange
  , ListIn d.inputs tx.inputs
  , ListIn d.outputs tx.outputs
  , ValueSubset d.mint tx.mint
  , SetSubset d.signatures tx.signatures
  , DPair f $ \f => (ListFiltered (\u => fst u === p) tx.inputs f, ListIn f d.inputs)
  , DPair f $ \f => (ListFiltered (\u => fst u === p) tx.outputs f, ListIn f d.outputs)
  , DPair f $ \f =>
    DPair f' $ \f' =>
    (lookup p tx.mint === f, lookup p d.mint === f', f === f')
  )

record Protocol where
  constructor MkProtocol
  datumType : Type
  permissible' : (self : ProtocolName) -> Set (TxDiagram self)

nameFor : Protocol -> ProtocolName
sameDatumType : (p : Protocol) -> (datumType (nameFor p) === p.datumType)

permissible : (p : Protocol) -> Set (TxDiagram (nameFor p))
permissible p = p.permissible' (nameFor p)

sameProtocol :
  (p1 : Protocol) ->
  (p2 : Protocol) ->
  (nameFor p1 === nameFor p2) ->
  p1 === p2

ReducibleProtocol : (p : Protocol) -> (UTXOFor (nameFor p) -> Type) -> Type
DisjointProtocol : Protocol -> Type
DisjointProtocol p =
  (x : (permissible p).b) -> 
  (y : (permissible p).b) -> (tx : Tx) -> Not (x === y) -> Not (TxMatches tx ((permissible p).f x), TxMatches tx ((permissible p).f y))
CoveredProtocol : Protocol -> Type

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

record CounterDatum where
  constructor MkCounterDatum
  counter : Nat
  pkh : PubKeyHash

data CounterProtocolPermissible : Type where
  CounterProtocolStep : List (DPair CounterDatum $ \datum -> LTE 1 datum.counter, Value) -> CounterProtocolPermissible -- why is this a dependent type? Second argument might be a predicate (datum -> Bool)?
  CounterProtocolConsume : (amount : Nat) -> (pkh : PubKeyHash) -> Value -> CounterProtocolPermissible

counterProtocolPermissible : CounterProtocolPermissible -> ProtocolName -> TxDiagram
counterProtocolPermissible (CounterProtocolStep list) self = MkTxDiagram {
  inputs = map (\((MkDPair datum _), value) -> MkTxIn { ref = Nothing, utxo = MkUTXO self value datum }) list,
  outputs = map (\((MkDPair datum _), value) -> MkTxOut { unique = (), utxo = MkUTXO self value (datum { counter = datum.counter - 1 }) }) list
}
counterProtocolPermissible (CounterProtocolConsume amount pkh value) = MkTxDiagram {
  inputs = replicate amount $ MkTxIn { ref = Nothing, utxo = MkUTXO self value (MkCounterDatum { counter = 0, pkh = pkh })},
  signatures = [ pkh ]
}

counterProtocol : Protocol
counterProtocol = MkProtocol {
  datumType = CounterDatum
  permissibleType = CounterProtocolPermissible
  permissible = counterProtocolPermissible
}

counterProtocolSoundness : ProtocolSoundness PSL.counterProtocol
counterProtocolSoundness = MkProtocolSoundness {
  reducibleType = \_ => (),
  disjointnessProof = ?osff,
  coverageProof = ?noideaatallwrtwhatthisshouldbehelpme,
  reducibleProof = ?
}

-- https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week10.html

-}
