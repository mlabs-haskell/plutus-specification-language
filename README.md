# Plutus Specification Language

The motivation for this language is to make plutus specifications, rigorous,
precise, easy and unambiguous. The language consists of two layers:

  1. a UTXO-level specification
  2. higher-level compositions of lower-level specs through abstraction and application

# Language

```
t0 = ...

t1 = ∀ { n : Int : n > 10 and n < 20, b : Bool }
   . { inputs: utxoRefs              -- list of UTXO inputs
     , outputs: utxos                -- list of UTXO outputs
     , validator:
        if b
          then n < 12
          else true
     , effects: effect               -- list of effects produced by this transaction
     , signatures: pubkey ↦ sig      -- map from pubkey hashes to transaction signatures
     , range: (expr, expr)           -- time range, specified as a tuple of slot numbers
     , ?fee: expr                    -- fee amount in Ada, optional field
     }



v1 =

tCombined = ∀ (r : Bool) (g : Bool) (b : Bool)

1. show equivalence between utxo validators + minting validators <=> transaction validators
  v1 and v2 <=> txnValidator
2. show equivalence between compositions of transaction validators <=> higher-level props
  t15 = t1 r -> t2 r -> (t3 r | t4) -> t5 :: forall r@{x: int} . ( ... )
  t16 = ...

  t15 -> t16 :: forall . (stuff I want) and not (stuff I don't want)

  forall (t : Transaction) .
    {
      inputs: t.outputs[i],
      outputs:
        { validator: ..., value: ..., datum: ... }
      validator: (v t.ouputs[1]) and ...
    }

v = \{address,...} -> ...
```

Our first-order speccing language:

```
spec ::=
  txn-def*
  validator-def*

-- transactions

txn-def ::=
    T = txn-expr                      -- transaction binding

txn-expr ::=
    abs . txn-expr                    -- transaction abstraction
  | quantifier-list .                 -- variable introduction, including redeemer inputs
      { inputs: utxoRefs              -- list of UTXO inputs
      , outputs: utxos                -- list of UTXO outputs
      , effects: effect               -- list of effects produced by this transaction
      , signatures: pubkey ↦ sig      -- map from pubkey hashes to transaction signatures
      , range: (expr, expr)           -- time range, specified as a tuple of slot numbers
      , ?forge: expr                  -- tokens to mint
      , ?fee: expr                    -- fee amount in Ada, optional field
      }
  | txn-comp -> txn-comp              -- sequential composition
  | ( txn-comp | txn-comp )           -- parallel composition
  | txn-expr : validator-expr         -- transaction validation
  | T                                 -- transaction variables

abs ::=
  λ(U : UTXO)* (T : Transaction)* (x : τ : ?expr)*    -- abstraction over transactions, utxos or values

quantifier-list ::=
  ∀ (x : τ : ?expr)*

-- UTXOs

utxoRefs ::=
    T.outputs[i]                    -- reference a utxo output from a previously defined transaction
  | utxoRefs , utxoRefs
  | ?utxoRefs
  | utxos

utxos ::=
    utxos , utxos
  | utxo
  | ?utxo                           -- optional utxo

utxo ::=
  {
  , validator: expr                -- validator scripts
  , value: φ,tok ↦ expr             -- token bundle
  , datum: (expr : τ)
  }
  |
  {
  , pubkey: pubkey                -- for pubkey payments
  , value: φ,tok ↦ expr             -- token bundle
  }

validator-def ::=
    v = validator-expr              -- validator definition

validator-expr ::=
  | abs . validator-expr            -- validator abstraction
  | validator x                     -- validator application to values
  | validator T                     -- validator application to transactions
  | validator U                     -- validator application to utxos
  | v                               -- validator variable
  | expr                            -- (boolean) expression

effect ::=
    mint φ tok expr                 -- mint expr tokens under minting policy φ
  | burn φ tok expr                 -- burn expr tokens under minting policy φ
  | effect , effect                 -- effect sequencing

policy ::=
  φ = quantifier-list . expr        -- spec for a minting policy

policy-id ::= φ

expr ::=
    x                               -- var
  | T                               -- transaction variable
  | T.outputs[i]                    -- UTXO reference
  | expr.datum                      -- where expr evaluates to a UTXO
  | quantifiers . expr              -- abstraction
  | expr expr                       -- application
  | expr + expr                     -- arithmetic
  | expr - expr
  | validator-expr                  -- use of other validators
  | ...
  | expr <> expr
  | expr and expr                   -- logical operators
  | expr or expr
  | not expr
  | currentSlot                     -- timeslots
  | cons expr expr                  -- lists
  | ...
  | ( expr )                        -- grouping
  | "..."                           -- string literals
  | [0-9]+                          -- integer literals
  | r@{ l1: expr, ..., ln: expr }   -- row expression
  | any                             -- any value (of any type)

τ ::=
    Int
  | Bool
  | String
  | ByteString
  | [τ]                             -- lists
  | (τ, τ)                          -- type conjunction
  | C τ \/ C τ                      -- type disjunction / variants
  | rt@{ l1: τ, ..., ln: τ }        -- row expression
  | Transaction                     -- transaction types
  | UTXO                            -- UTXO types

rulename ::= x

num ::= [0..9]+
string-literals ::= "..."

with the following meta-variables:

    σ ranges over specifications
    T ranges over transaction variables
    U ranges over UTXO variables
    α ranges over addresses
    τ ranges over types
    r ranges over row variables
    rt ranges over row type variables
    φ ranges over minting policies and indicates the type of token
    C ranges over data constructor names
    x ranges over variable names (a-z, A-Z, 0-9, _)
    i ranges over positive integers
    txn is a built-in variable that references the currently pending transaction in a validator expression
    tok ranges over token names
    pubkey ranges over public key hashes
    sig ranges over signatures
    slot ranges over block numbers
```

# Higher-Order Speccing Language

We extend our speccing language to include higher-order specifications
using the following constructs:

```
rules ::=
    ...
  | \(x : σ).rules  -- abstraction
  | rules rules     -- application
```

# Example

# Tooling

Currently there is no tooling around this language, but we have a number of ideas:

  1. test suite generator
  2. spec typechecking (ensuring specs are sound)
  3. symbolic execution engine to match the implementation (untyped plutus core)
     against the spec (with answers `Yes`, `No` with a reason, or `Maybe`)
  4. fuzzing tools
  5. model checking code generators
  6. other code generators to formal languages (e.g. Agda, Idris, Coq, etc)

