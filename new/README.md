# High-level specification language

## Motivation / Definition

We want to talk about the subset of protocols on Cardano that are *sensible*.

What is a protocol? We define a simplified model of Cardano for this.

A transaction is a set of *inputs* (i.e. unordered list), a set of *outputs*,
the *minted/burned* value, and a *validity range* (pair of time).

Inputs and outputs contain value and are locked by a *protocol*.

```idris
record Tx where
  constructor MkTx
  inputs : Set TxIn
  outputs : List TxOut
  mint : Value
  signatures : Set PubKeyHash
  validRange : POSIXTimeRange
```

This definition might be expanded to include more of the underlying transaction's
information.

*Protocols* are essentially sets of permissible transaction.
They are modelled as so:
```idris
record ProtocolDef where
  constructor MkProtocolDef
  datumType : Type
  permissibleType : Type
  permissible : permissibleType -> (self : ProtocolIdentity) -> Tx

identityFor : ProtocolDef -> ProtocolIdentity

record Protocol
  constructor MkProtocol
  def : ProtocolDef
  toPreserveType : datumType -> Type
  noOverlapProof : (x : permissibleType) -> (y : permissibleType) -> (tx : Tx) -> Not (txMatches def tx x, txMatches def tx y)
  reducibleProof : ?reducibleProofType
```

Let us now define transaction validity (we ignore protocol limits for this):
A transaction `t` is valid if for every protocol `p` that it references,
there is an `x : p.permissibleType` such that `let t' = p.permissible x p` is
such that the inputs and outputs of `t'` and `t` that are locked by `p` match exactly.
The tokens of `p` minted by `t'` and `t` must also match exactly.
It must also be that for all fields of `t'`, they must be "subsets of" the
corresponding fields of `t`.
Any inputs or outputs that `t'` references of other protocols, must also be present
in `t` (but not necessarily vice versa).
Any signature in `t'` must also be in `t`.
Anything minted in `t'` must also be minted in `t`.
`validRange` must match exactly.

The following is true by definition:
Given a transaction that references one set of protocols,
and a transaction that references another set of protocols,
such that they are disjoint sets of protocols, then
provided that the `validRange` matches exactly,
those two transactions can be combined in such a way that
the combined transaction has the same effect (consumes
the same UTXOs and outputs equivalent UTXOs), given
that the signatures are also combined from both transactions.
