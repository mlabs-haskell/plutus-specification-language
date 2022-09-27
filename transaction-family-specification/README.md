# Transaction Family Specification

This is a small library for writing Haskell-type-level specifications of Cardano dApps and transaction families. To
find out more, visit documentation:

* [Introduction](doc/TransactionFamily.md)
* [Implementation Sketch](doc/Typed.md)
* [Working Implementation](doc/HKD.md)
* [Value Level](doc/Values.md)
* [Diagram Generation](doc/Diagram.md)

## Prerequisite

You'll need GHC 9. Nix is neither required nor wanted in this directory, so `exit` from `nix develop` if you're inside
it or open a different terminal to go here. Then `ghcup set ghc 9.2.4` or whichever specific version you have installed.

## Usage

1. In order to depend on the library, you can start by copying the contents of the [`scaffold`](scaffold/) directory.
2. Then replace and fill out the `DApp`, `TransactionFamily`, and `Token` data ~type~kind declarations, listing as
their constructors respectively the scripts, transactions, and token types present in your dApp.
3. Write an `instance MintingPolicyScript 'MyMintingPolicy` declaration for every minting policy script listed as a
   constructor of your `DApp` data kind.
4. Write an `instance ValidatorScript 'MyValidator` declaration for every validator script listed as a constructor of
   your `DApp` data kind.
5. Write an `instance Transaction 'MyTransaction` declaration for every transaction type listed as a constructor of
   your `TransactionFamily` data kind.
6. Once everything compiles, you may want to generate some [diagrams](doc/Diagram.md) and check if they make sense.
7. Eventually there should be some useful value-level stuff to instantiate concrete, type-checked transaction values.

## Build

```sh
cabal build
```

## Test documentation

```sh
cabal test
```

## Generate example diagrams

```sh
cabal run
```
