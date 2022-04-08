module PSL

import Data.List
import Data.DPair
import Data.Vect

%default total

namespace Set
  public export
  record Set (a : Type) where
    constructor MkSet
    0 indexType : Type
    index : indexType -> a

  public export
  0 SetSubsetF : (a -> Type) -> Set a -> Set a -> Type
  SetSubsetF f x y = (z : x.indexType) -> f (x.index z) -> DPair y.indexType (\w => (f (y.index w), x.index z === y.index w))

  public export
  0 SetSubset : Set a -> Set a -> Type
  SetSubset = SetSubsetF (\_ => ())

  map : (a -> b) -> Set a -> Set b
  map f (MkSet t g) = MkSet t (f . g)

  public export
  fromList : List a -> Set a
  fromList l =
    let l' : Vect (length l) a = fromList l in
    MkSet (Fin (length l)) (\i => index i l')

  public export
  0 SetAll : (a -> Type) -> Set a -> Type
  SetAll f x = (y : x.indexType) -> f (x.index y)

  public export
  0 SetFiltered : (a -> Type) -> Set a -> Set a -> Type
  SetFiltered f x y =
    ( SetSubset y x
    , SetSubsetF f x y
    , SetAll f y
    )

public export
ProtocolName : Type -> Type

public export
protocolEquality : ProtocolName a -> ProtocolName b -> Bool

public export
protocolEqualityReflexivity : (p : ProtocolName a) -> (protocolEquality {a = a} {b = a} p p === True)

public export
protocolEqualitySymmetry : (p : ProtocolName a) -> (q : ProtocolName b) -> (protocolEquality {a} {b} p q === protocolEquality {a = b} {b = a} q p)

public export
protocolEqualityTransitivity : (p : ProtocolName a) -> (q : ProtocolName b) -> (r : ProtocolName c) -> protocolEquality {a} {b} p q === True -> protocolEquality {a = b} {b = c} q r === True -> protocolEquality {a} {b = c} p r === True

public export
UTXORef : Type

public export
TokenName : Type

public export
PubKeyHash : Type

namespace Map
  public export
  Map : Type -> Type -> Type

  public export
  NilMap : Map a b

  public export
  lookupMap : a -> Map a b -> Maybe b

public export
Value : Type
Value = Map (Exists ProtocolName) (Map TokenName Integer)

namespace UTXO
  public export
  record UTXO {0 d : Type} (p : ProtocolName d) where -- SomeUTXO *for a specific protocol*.
    constructor MkUTXO
    value : Value
    datum : d
    staker : Maybe (Exists ProtocolName) -- FIXME: We don't have a way of defining the protocol

  public export
  protocol : {p : ProtocolName d} -> UTXO {d = d} p -> ProtocolName d
  protocol {p} _ = p

namespace SomeUTXO
  public export
  SomeUTXO : Type
  SomeUTXO = DPair (Exists ProtocolName) (\(Evidence d p) => UTXO {d = d} p)

  public export
  MkSomeUTXO : {0 d : Type} -> {p : ProtocolName d} -> UTXO {d} p -> SomeUTXO
  MkSomeUTXO {d} {p} u = MkDPair (Evidence d p) u

  public export
  protocol : (utxo : SomeUTXO) -> ProtocolName (fst $ fst utxo)
  protocol utxo = snd $ fst utxo

public export
record TxIn where
  constructor MkTxIn
  utxo : SomeUTXO
  ref : Maybe UTXORef

namespace TxOut
  public export
  record TxOut {0 d : Type} (context : ProtocolName d) where
    constructor MkTxOut
    {0 d' : Type}
    {protocol : ProtocolName d'}
    0 isDifferentProtocol : (protocolEquality {a = d} {b = d'} context protocol === False)
    utxo' : UTXO {d = d'} protocol
    unique : Bool

  public export
  utxo : TxOut context -> SomeUTXO
  utxo txout = MkDPair (Evidence txout.d' txout.protocol) txout.utxo'

public export
TimeRange : Type

public export
record TxDiagram {0 d : Type} (context : ProtocolName d) where
  constructor MkTxDiagram
  inputs : Set TxIn
  ownOutputs : List (UTXO {d} context)
  otherOutputs : Set (TxOut {d} context)
  mint : Value
  signatures : Set PubKeyHash
  validRange : TimeRange

-- Order is important
public export
data ListIn : List a -> List a -> Type where
  MkListInPrefix : (x : List a) -> (y : List a) -> (z : List a) -> ListIn x (x ++ y ++ z)

public export
data ListFiltered : (a -> Type) -> List a -> List a -> Type where
  MkListFilteredNil : ListFiltered f Nil Nil
  MkListFilteredCons : {0 f : a -> Type} -> f h -> ListFiltered f xt yt -> ListFiltered f (h :: xt) (h :: yt)
  MkListFilteredSkip : {0 f : a -> Type} -> Not (f h) -> ListFiltered f xt y -> ListFiltered f (h :: xt) y

public export
ValueSubset : Value -> Value -> Type

public export
record Tx where
  constructor MkTx 
  inputs : Set TxIn
  outputs : List SomeUTXO
  mint : Value
  signatures : Set PubKeyHash
  validRange : TimeRange

-- FIXME: We should have a lenient version to allow lenient implementations
-- that still achieve the same goal.
-- FIXME: Consider input references
public export
0 TxMatches : {0 d : Type} -> {p : ProtocolName d} -> Tx -> TxDiagram {d} p -> Type
TxMatches {p} tx diagram =
  ( tx.validRange === diagram.validRange
  , SetSubset diagram.inputs tx.inputs
  -- FIXME this is wrong
  -- FIXME we need to check otherOutputs too
  , ListIn (map MkSomeUTXO diagram.ownOutputs) tx.outputs
  , ValueSubset diagram.mint tx.mint
  , SetSubset diagram.signatures tx.signatures
  , SetSubsetF (\inp => protocolEquality (protocol inp.utxo) p === True) tx.inputs diagram.inputs
  , DPair (Maybe (Map TokenName Integer)) $ \f =>
    DPair (Maybe (Map TokenName Integer)) $ \f' =>
    (lookupMap (Evidence d p) tx.mint === f, lookupMap (Evidence d p) diagram.mint === f', f === f')
  )

-- FIXME: Consider input references
public export
0 TxMatchesLenient : {0 d : Type} -> {p : ProtocolName d} -> Tx -> TxDiagram {d} p -> Type
TxMatchesLenient {p} tx diagram =
  ( tx.validRange === diagram.validRange
  , SetSubset diagram.inputs tx.inputs
  , SetSubset (fromList $ map MkSomeUTXO $ diagram.ownOutputs) (fromList tx.outputs)
  , SetSubset (map utxo diagram.otherOutputs) (fromList $ tx.outputs)
  , ValueSubset diagram.mint tx.mint
  , SetSubset diagram.signatures tx.signatures
  , SetSubsetF (\inp => protocolEquality (protocol inp.utxo) p === True) tx.inputs diagram.inputs
  -- FIXME: this is incorrect
  , DPair (Maybe (Map TokenName Integer)) $ \f =>
    DPair (Maybe (Map TokenName Integer)) $ \f' =>
    (lookupMap (Evidence d p) tx.mint === f, lookupMap (Evidence d p) diagram.mint === f', f === f')
  )

public export
record Protocol where
  constructor MkProtocol
  datumType : Type
  permissible' : (self : ProtocolName datumType) -> Set (TxDiagram {d = datumType} self)

public export
nameFor : (p : Protocol) -> ProtocolName p.datumType

public export
permissible : (p : Protocol) -> Set (TxDiagram {d = p.datumType} $ nameFor p)
permissible p = p.permissible' (nameFor p)

public export
ReducibleProtocol : (p : Protocol) -> (UTXO {d = p.datumType} (nameFor p) -> Type) -> Type

public export
0 DisjointProtocol : Protocol -> Type
DisjointProtocol p =
  (x : (permissible p).indexType) ->
  (y : (permissible p).indexType) ->
  (tx : Tx) ->
  Not (x === y) ->
  Not (TxMatches tx ((permissible p).index x), TxMatches tx ((permissible p).index y))

public export
CoveredProtocol : Protocol -> Type

public export
record ProtocolSoundness (protocol : Protocol) where
  constructor MkProtocolSoundness
  reducibleType : UTXO (nameFor protocol) -> Type
  reducibility : ReducibleProtocol protocol reducibleType
  disjointness : DisjointProtocol protocol
  coverage : CoveredProtocol protocol
