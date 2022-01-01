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
