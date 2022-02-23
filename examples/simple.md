# Super simple DEX

Our goal is to allow simple exchanges between two separate Cardano values,.

Our protocol consists of 1 script.

A user locks their funds with this script, along with a datum that
specifies the value they want in exchange and what PKH to send it to.

The UTXO can be consumed if the consuming transaction pays the specified value
to the specified PKH, in a UTXO where the datum contains the TxOutRef of the
UTXO that contained the initial user's funds.

The user can also retrieve their funds if they sign the transaction
with the PKH specified.

## Pseudo-code

```haskell
\datum _ ctx ->
  -- strict let
  let (value, pkh) = parseDatum datum in
  (||) (any (== pkh) ctx.info.signatories) $
    let Spending myref = ctx.purpose in
    let f out =
        out.address == Address { credential = PubKeyCredential pkh, stakingCredential = Nothing }
        && out.datumHash == hash . toData $ myref
        && out.value == value
    in
    any f ctx.info.outputs
```

## Spec

There are a lot of issues with the specification language.
- No type definitions
- No types for many built-in types
- No `let` in expressions
- No proper HOFs
- No `case of`
- Too much hard-coded syntax instead of expressions with types
- Transactions with variable number of inputs impossible
- Not possible to refer to `TxOutRef` for input
- Fees?
- Minimum Ada limit?
- No `Data`

How do we write a useful spec that encapsulates our design goals?

```
Value = ...
Address = ...
PubKeyHash = ...
TxInfo = ...
ScriptPurpose = ...

MyDatum = {value : Value, pkh : PubKeyHash}
MyRedeemer = {value : Value, pkh : PubKeyHash}

thevalidator = λ(datum : MyDatum) (redeemer : MyRedeemer) (purpose : ScriptPurpose) (tx : TxInfo).
  any (λ(x : PubKeyHash). x == datum.pkh) tx.signatories or
    case ctx.purpose of
      _ -> False
      Spending myref ->
        let f = λ(out : UTXO)
            out.address == { credential = PubKeyCredential datum.pkh, stakingCredential = Nothing }
            and out.datumHash == hashData . toData $ myref
            and out.value == value
        in
        any f ctx.info.outputs

t0 = ∀ (indatum : ???) (inaddr : Address) (give : Value) (take : Value) (key : PubKeyHash). {
  inputs:
    {
      address: inaddr,
      value: take,
      datum: indatum,
    },
    {
      address: thevalidator,
      value: give,
      datum: ({value: take, pkh: key} : MyDatum),
    },
  outputs:
    {
      address: pkh,
      value: take,
      datum: FIXME,
    },
  effects: FIXME,
  signatures: FIXME,
  range: FIXME,
  fee: FIXME
}
```

## What we really want

MAX = 14 KiB

- For any UTXO locked with our script and a datum of the correct format of size less than MAX,
  if we know the private key that corresponds to the public key hash in its datum,
    then we must be able to consume it iff we can pay the fees.
    else we must be able to consume it iff we pay the value specified
      to the public key hash specified and are able to pay the fees.
- For any UTXOs locked with our script and a datum of the correct format, if we are able to consume them
  individually, we must be able to consume them together if they fit within
  the transaction limits.

How do we specify this?

## Proof layers

One attempt sketch, following the [Proof Architecture diagram](../README.md#general-design).


### Protocol Theorems

Here's one way to specify the first bullet point of [what we really want](#what-we-really-want):

```
∀ (o :: UTxO SimpleDexDatum) (w :: Wallet) (s :: SigningKey).
  o `LockedBy` SimpleDexValidator ∧ CanCoverAllFeesAndCollateral w ∧ o.datum.pkh `Witnesses` s ⇒
    ∃ (t :: TxInfo).
      t `IsSignedBySecret` s ∧ w `CoversFeesAndCollateralFor` t ∧ t `Disburses` o.value ∧ t `Spends` o

∀ (o :: UTxO SimpleDexDatum) (w :: Wallet) (p :: PaymentPublicKey).
  o `LockedBy` SimpleDexValidator ∧ CanCoverAllFeesAndCollateral w ∧ o.datum.pkh `IsHashOf` p ⇒
    ∃ (t :: TxInfo).
      PaysToPubKey t p o.datum.value ∧ w `CoversFeesAndCollateralFor` t ∧ t `Disburses` o.value ∧ t `Spends` o
```

The second bullet point is a bit awkwardly specified and hints at a
semigroup-like property of transactions. We'd have to assume a theorem
provided by the Induction Library (even if there's no induction involved in
this particular case):

```
∀ (ts :: [Transaction]) (vs :: [SomeValidator]). All MindsOwnBusiness vs ⇒
  All (\t-> All (InputsOf t) (\o-> Any (o `LockedBy`) vs)) ts ⇒
    UnionOf ts `SpendsAll` Concat (InputsOf t)
```

Anyway for this simple validator the semigroup property is not necessary:

- if the transaction is signed by our private key we can consume any number of
  UTxO locked by the validator and specifying the corresponding public key hash,

- else we must be able to consume any number of UTxOs locked by the validator
  iff we pay the value specified to the public key hash specified by each and
  are able to pay the fees.

```
∀ (os :: [UTxO SimpleDexDatum]) (w :: Wallet) (s :: SigningKey).
  All (\o-> o `LockedBy` SimpleDexValidator ∧ o.datum.pkh `Witnesses` s) os ∧ CanCoverAllFeesAndCollateral w ⇒
    ∃ (t :: TxInfo).
      t `IsSignedBySecret` s ∧ w `CoversFeesAndCollateralFor` t
      ∧ All (\o-> t `Disburses` o.value ∧ t `Spends` o) os

∀ (ops :: [(UTxO SimpleDexDatum, PaymentPublicKey)]) (w :: Wallet).
  All (\(o, p)-> o `LockedBy` SimpleDexValidator ∧ o.datum.pkh `IsHashOf` p) ops ∧ CanCoverAllFeesAndCollateral w ⇒
    ∃ (t :: TxInfo).
      w `CoversFeesAndCollateralFor` t
      ∧ All (\(o, p)-> PaysToPubKey t p o.datum.value ∧ t `Disburses` o.value ∧ t `Spends` o) ops
```

### Transaction Lemmas

... are really the same as protocol theorems here because in this simple
protocol there's no dependency between transactions.

### Validator Script Lemmas

These are fairly simple, but should be checked against the actual validator
code. Ideally in its final UPLC form and automatically.

```
∀ (o :: UTxO SimpleDexDatum) (t :: TxInfo).
  ApprovesSpending SimpleDexValidator t o
  ⟺ (t `IsSignedBy` o.datum.pkh ∨ PaysToPubKeyHash t o.datum.pkh o.datum.value)

∀ (o :: UTxO SimpleDexDatum) (t :: TxInfo). o `LockedBy` SimpleDexValidator ⇒
  t `Spends` o ⇒ (t `IsSignedBy` o.datum.pkh ∨ PaysToPubKeyHash t o.datum.pkh o.datum.value)
```

And of course we mustn't forget

```
MindsOwnBusiness SimpleDexValidator
```
