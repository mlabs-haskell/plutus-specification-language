# Plutus Transaction Families

## Concepts and Terminology

The usual Cardano/Plutus concepts presumably need no introduction:
  * *Script*
  * *UTxO*, or Unspent TransaXion Output
  * *Transaction*

<!--

~~~ {.haskell}
{-# LANGUAGE EmptyDataDeriving #-}

import Data.Either (rights)
import Data.List (intersect)
import Typed ()

main = pure ()

data Script deriving (Eq)
data PubKeyHash
data UTxO
data Redeemer
data TokenName
data Transaction
~~~
-->

Note that we elide the difference between `Script`, `MintingPolicy`,
`Validator`, `TypedValidator`, `ScriptHash`, and script `Address` for the time
being, and we also ignore staking. We can now assume the existence of functions

~~~ {.haskell}
utxoAddress :: UTxO -> Either PubKeyHash Script
transactionInputs :: Transaction -> [(UTxO, Redeemer)]
transactionMints :: Transaction -> [(Script, Redeemer, [(TokenName, Integer)])]
transactionOutputs :: Transaction -> [UTxO]
~~~

<!--
~~~ {.haskell}
utxoAddress = undefined
transactionInputs = undefined
transactionMints = undefined
transactionOutputs = undefined
~~~
-->

From these we can define

~~~ {.haskell}
allScriptsOf :: Transaction -> [Script]
allScriptsOf t =
  rights (utxoAddress <$> ((fst <$> transactionInputs t) <> transactionOutputs t))
  <> (fstOf3 <$> transactionMints t)
  where fstOf3 (x, _, _) = x
  
related :: Transaction -> Transaction -> Bool
related t1 t2 = not $ null $ allScriptsOf t1 `intersect` allScriptsOf t2
~~~

Now we can extend the set of concepts as follows:

* A *transaction family* is a set of transitively `related` transactions;
  more formally, `∀ t1 t2 ∈ F. transitively-related t1 t2` where
  `transitively-related t1 t2 = related t1 t2 ∨ (∃t. related t1 t ∧
  transitively-related t t2)`.
* A *DApp* or Distributed Application is a union of all scripts of all
  transactions belonging to a transaction family: `dApp fam = foldMap
  ``allScriptsOf`` fam`
* If we can prove that no transaction outside a given family can
  successfully invoke a script from the family's DApp, we can call the
  family and its DApp *closed*.
* A *family protocol* is a set of all valid sequences of transactions
  belonging to a given family. Another name for *set of all valid sequences*
  is *language*, and any such language should ideally be specified as a
  *formal grammar*. The term *transaction family language* would only invite
  confusion, so we'll stick with *protocol* and *protocol grammar*.

Some DApps could be kept intentionally non-closed, i.e. open. For example a
script may allow any transaction signed by a predetermined authority to pass,
no further questions asked. A trustworthy design should keep such transactions
exceptional or eliminate them altogether. A more legitimate example would be a
DApp upgrade protocol, which allows for introduction of new scripts and
migration of UTxOs from old to new scripts. The inclusion of an upgrade
protocol makes a DApp open by definition, because we cannot tell which
transactions will be allowed by a future script. All we can do is ensure that
the openings are well understood and well guarded.

Next we add [types](Typed.md).
