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

  Functor Set where
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
  0 SetHas : Set a -> a -> Type
  SetHas s x = Exists (\idx => s.index idx === x)

  0 SetEqual : Set a -> Set a -> Type
  SetEqual x y = (SetSubset x y, SetSubset y x)

public export
UTXORef : Type

public export
Data : Type

public export
Hash : Type

public export
ByteString : Type

public export
serialiseData : Data -> ByteString
public export
hash : ByteString -> Hash
public export
hashAsByteString : Hash -> ByteString

public export
Credential : Type

public export
TokenName : Type

public export
PubKeyHash : Type

public export
TimeRange : Type

public export
Address : Type

namespace RealUTXO
  public export
  RealUTXO : Type

  public export
  credential : RealUTXO -> Credential

public export
UTXORefIs : UTXORef -> RealUTXO -> Type

public export
ScriptHash : Type

public export
Script : Type

public export
data AdditiveNat : Type where
  MkAdditiveNat : Nat -> AdditiveNat

Semigroup AdditiveNat where
  MkAdditiveNat x <+> MkAdditiveNat y = MkAdditiveNat $ x + y

Monoid AdditiveNat where
  neutral = MkAdditiveNat Z

Eq AdditiveNat where
  MkAdditiveNat x == MkAdditiveNat y = x == y

public export
data AdditiveInteger : Type where
  MkAdditiveInteger : Integer -> AdditiveInteger

Semigroup AdditiveInteger where
  MkAdditiveInteger x <+> MkAdditiveInteger y = MkAdditiveInteger $ x + y

Eq AdditiveInteger where
  MkAdditiveInteger x == MkAdditiveInteger y = x == y

Monoid AdditiveInteger where
  neutral = MkAdditiveInteger 0

namespace MonoidMap
  public export
  MonoidMap : (keyty : Type) -> (valty : Type) -> Monoid valty => Type

  public export
  lookup : Monoid v => k -> MonoidMap k v -> v

  public export
  insert : Monoid v => k -> v -> MonoidMap k v -> MonoidMap k v

  Monoid v => Semigroup (MonoidMap k v) where
    x <+> y = ?appendMonoidMap

  Monoid v => Semigroup (MonoidMap k v) => Monoid (MonoidMap k v) where
    neutral = ?emptyMonoidMap

  (Monoid v, Eq v) => Eq (MonoidMap k v) where
    x == y = ?eqMonoidMap

  public export
  MonoidMapMonoid : Monoid v -> Monoid (MonoidMap k v)

  --public export
  --lookupLaw : (valtymon : Monoid valty) => let _ = MonoidMapMonoid valtymon in (key : keyty) -> PSL.MonoidMap.lookup key (the (MonoidMap k v) Prelude.neutral) === the valty Prelude.neutral

namespace Value
  public export
  Value : Type
  -- Idris is broken
  Value = Monoid (MonoidMap TokenName AdditiveInteger) => MonoidMap ScriptHash (MonoidMap TokenName AdditiveInteger)

  public export
  NatValue : Type
  NatValue = Monoid (MonoidMap TokenName AdditiveNat) => MonoidMap ScriptHash (MonoidMap TokenName AdditiveNat)

namespace UTXO
  public export
  record UTXO (0 d : Type) where -- SomeUTXO *for a specific protocol*.
    constructor MkUTXO
    value : NatValue
    datum : d
    staker : Maybe Credential -- FIXME: We don't have a way of defining the protocol

public export
record TxDiagram (0 d : Type) where
  constructor MkTxDiagram
  observeInputs : Set UTXORef
  causeOwnInputs : Set (UTXO d)
  causeOwnOutputs : Set (UTXO d)
  observeOutputs : Set RealUTXO
  causeOutputs : Set RealUTXO
  causeOwnMint : MonoidMap TokenName AdditiveInteger
  observeMint : Value
  signatures : Set PubKeyHash
  validRange : TimeRange

public export
record Protocol where
  constructor MkProtocol
  datumType : Type
  cases : Set (TxDiagram datumType)

record ProtocolImplementation (p : Protocol) where
  constructor MkProtocolImplementation
  targetValidators : Set Credential
  recognise : RealUTXO -> Bool
  recogniseLaw : (u : RealUTXO) ->
    ( SetHas targetValidators (credential u) ->
      recognise u === True
    , Not (SetHas targetValidators (credential u)) ->
      recognise u === False
    )
  mapUTXO : UTXO p.datumType -> RealUTXO
  mapUTXOLaw :
    (u : UTXO p.datumType) ->
    Exists
      (\idx => credential (mapUTXO u) === targetValidators.index idx)
  mapToken : TokenName -> (ScriptHash, TokenName)
  identity : ByteString

utxoFor : (0 p : Protocol) -> {auto pimpl : ProtocolImplementation p} -> UTXO p.datumType -> RealUTXO
utxoFor _ u = pimpl.mapUTXO u

tokenFor : (0 p : Protocol) -> {auto pimpl : ProtocolImplementation p} -> TokenName -> (ScriptHash, TokenName)
tokenFor _ t = pimpl.mapToken t

namespace Tx
  public export
  Tx : Type

  public export
  inputs : Tx -> Set UTXORef
  public export
  outputs : Tx -> Set RealUTXO
  public export
  mint : Tx -> Value
  public export
  signatures : Tx -> Set PubKeyHash
  public export
  validRange : Tx -> TimeRange

UTXOInTx : Tx -> RealUTXO -> Type

-- Avoids double satisfaction attack.
-- Script checks that all other inputs are PKH inputs.
-- Datum is a pair of the protocol and the original datum.
-- There must be an output with the same value and original datum.
causeUTXO : ByteString -> RealUTXO -> RealUTXO

-- TODO: Find what correct ProtocolImplementation is
-- FIXME: Consider input references
public export
0 TxMatches : {0 p : Protocol} -> {auto pimpl : ProtocolImplementation p} -> Tx -> TxDiagram p.datumType -> Type
TxMatches tx diagram =
  let
    realOwnInputs : Set RealUTXO
    realOwnInputs = map (utxoFor p) diagram.causeOwnInputs
    realOwnOutputs : Set RealUTXO
    realOwnOutputs = map (utxoFor p) diagram.causeOwnOutputs
  in
  ( validRange tx === diagram.validRange
  , ( inputs' : Set (Subset (UTXORef, RealUTXO) (uncurry UTXORefIs)) **
      ( SetEqual (map (fst . fst) inputs') (inputs tx)
      , SetSubset realOwnInputs (map (snd . fst) inputs')
      , SetSubsetF (\u => SetHas pimpl.targetValidators (credential u)) (map (snd . fst) inputs') realOwnInputs
      )
    )
  , SetSubset diagram.observeInputs (inputs tx)
  , SetSubset realOwnOutputs (outputs tx)
  , SetSubset diagram.observeOutputs (outputs tx)
  , SetSubset (map (causeUTXO pimpl.identity) diagram.causeOutputs) (outputs tx)
  , SetSubset diagram.signatures (signatures tx)
  -- FIXME: handle mint
  )

UTXOConsumable : Tx -> RealUTXO -> Type
Passes : Tx -> Type

0 CorrectProtocolImplementation : (p : Protocol) -> (pimpl : ProtocolImplementation p) => Type
CorrectProtocolImplementation p =
  ( (u : UTXO p.datumType) ->
    (t : Tx) ->
    UTXOInTx t (pimpl.mapUTXO u) ->
    let pm = cases p in
    (diagramIndex : pm.indexType) ->
    let diagram = pm.index diagramIndex in
    ( TxMatches t diagram -> UTXOConsumable t (pimpl.mapUTXO u)
    , Passes t -> TxMatches t diagram)
-- FIXME handle tokens
  )  -- not (TxMatches t diagram) -> Passes t -> Bottom

public export
ReducibleProtocol : (p : Protocol) -> (UTXO p.datumType -> Type) -> Type

public export
0 DisjointProtocol : (p : Protocol) -> Type
DisjointProtocol p =
  ProtocolImplementation p =>
  (x : p.cases.indexType) ->
  (y : p.cases.indexType) ->
  (tx : Tx) ->
  Not (x === y) ->
  Not (TxMatches tx (p.cases.index x), TxMatches tx (p.cases.index y))

public export
CoveredProtocol : Protocol -> Type

public export
record ProtocolSoundness (protocol : Protocol) where
  constructor MkProtocolSoundness
  reducibleType : UTXO protocol.datumType -> Type
  reducibility : ReducibleProtocol protocol reducibleType
  disjointness : DisjointProtocol protocol
  coverage : CoveredProtocol protocol
