module PSL

import Data.List
import Data.List.Elem

%default total

ProtocolName : Type

datumType : ProtocolName -> Type

UTXORef : Type

TokenName : Type

Map : (x : Type) -> (x -> Type) -> Type

Value' = Map TokenName (\_ => Integer)

Value = Map ProtocolName (\p => Value' p)

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
  unique : if context == (snd utxo).protocol then () else Bool

TimeRange : Type

record TxDiagram (context : ProtocolName) where
  constructor MkTxDiagram
  inputs : List TxIn
  outputs : List (TxOut context)
  mint : Value
  signatures : List PubKeyHash
  validRange : TimeRange

data Subset : Type -> Type where
  MakeSubset : (a : Type) -> (a -> Type) -> Subset a

{-
-- subset of two concrete lists? or should it be subset of types?
data ListSubset : Type -> Type -> Type where
  subset : 
    (a : Type) -> 
    (b : Type) ->
    (sa : List a) ->
    (sb : List b) ->      
    (_ : \(x : a) => elem x sa -> elem x sb) ->
    Subset a b
-}

ListSubset : Subset (List a) 

ListFiltered : (a -> Type) -> List a -> List a -> Type

lookup : (x : a) -> Map a b -> Maybe (b x) -- why (b x)?

-- fixme
SetSubset : Subset (List a)

ValueSubset : Subset Value

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
  , ListSubset d.inputs tx.inputs
  , ListSubset d.outputs tx.outputs
  , ValueSubset d.mint tx.mint
  , SetSubset d.signatures tx.signatures
  , DPair f $ \f => (ListFiltered (\u => fst u === p) tx.inputs f, ListSubset f d.inputs)
  , DPair f $ \f => (ListFiltered (\u => fst u === p) tx.outputs f, ListSubset f d.outputs)
  , DPair f $ \f =>
    DPair f' $ \f' =>
    (lookup p tx.mint === f, lookup p d.mint === f', f === f')
  )

record Set (a : Type) where
  constructor MkSet
  b : Type
  f : b -> a

getByKey : (a : Type) -> (b : Type) -> b -> Set a -> Maybe a
getByKey t keyType key (MkSet t f)
  | t == keyType = Just (f key)
  | otherwise = Nothing

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

ReducibleProtocol : (p : Protocol) -> (UTXOFor p -> Type) -> Type
DisjointProtocol : Protocol -> Type
DisjointProtocol p =
  (x : (permissible p).b) -> 
  (y : (permissible p).b) -> (tx : Tx) -> Not (x === y) -> Not (TxMatches tx ((permissible p).f x), TxMatches tx ((permissible p).f y))
CoveredProtocol : Protocol -> Type

record ProtocolSoundness (protocol : Protocol) where
  constructor MkProtocolSoundness
  reducibleType : UTXOFor protocol -> Type
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