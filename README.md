# Plutus Specification Language

The motivation for this language is to make plutus specifications, rigorous,
precise, easy and unambiguous. The language consists of two layers:

  1. a UXTO-level specification
  2. higher-level compositions of lower-level specs through abstraction and application

# Language

Our first-order speccing language:

```
spec ::=
  σ = rules                         -- assignment

rules ::=
    rules , rules                   --
  | rule

rule ::=
    txn -> txn                      -- transaction sequencing

txn ::=
  T =                               -- transaction binding
    quantifier-list .               -- variable bindings, including redeemer inputs
    { inputs: utxoRefs              -- list of UTXO inputs
    , outputs: utxos                -- list of UTXO outputs
    , validator: validator          -- validator
    , effects: effect               -- list of effects produced by this transaction
    , signatures: pubkey ↦ sig      -- map from pubkey hashes to transaction signatures
    , range: (expr, expr)           -- time range, specified as a tuple of slot numbers
    , ?forge: expr
    , ?fee: expr                    -- fee amount in Ada, optional field
    }

quantifier-list ::=
  ∀ binders

binders ::=
    (x : τ : ?expr)
  | binders binders

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
  , address: ?pubkey                -- for pubkey payments
  , value[φ]: expr
  , datum: (expr : τ)
  , pubkey
  }

validator ::=
    quantifier-list . expr          -- validator abstraction
  | validator x                     -- validator application
  | v

validator-def ::=
    v = validator

effect ::=
    mint φ expr                     -- mint expr tokens under minting policy φ
  | burn φ expr                     -- burn expr tokens under minting policy φ
  | effect , effect

policy ::=
  φ = expr                          -- spec for a minting policy

policy-id ::= φ

expr ::=
    x                               -- var
  | quantifiers . expr              -- abstraction
  | expr expr                       -- application
  | expr + expr                     -- arithmetic
  | expr - expr
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
  | any                             -- any value (of any type)

τ ::=
    Int
  | Bool
  | String
  | ByteString
  | [τ]                             -- lists
  | (τ, τ)                          -- type conjunction
  | C τ \/ C τ                      -- type disjunction / variants
  | Transaction

rulename ::= x

num ::= [0..9]+
string-literals ::= "..."

with the following meta-variables:

    σ ranges over specifications
    T ranges over transactions
    α ranges over addresses
    τ ranges over types
    φ ranges over minting policies and indicates the type of token
    C ranges over data constructor names
    x ranges over variable names (a-z, A-Z, 0-9, _)
    txn is a built-in variable that references the currently pending transaction in a validator expression
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

