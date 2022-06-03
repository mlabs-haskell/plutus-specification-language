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

## Formalization

So we have added some superstructure on top of Cardano concepts, but how does
it help?

~~~ {.haskell.ignore}

type TransactionKind = (TransactionInputs, TransactionMint, TransactionOutputs)

type TransactionInputs = [(Type, Type, Type)] -- (currency, datum, redeemer)

type TransactionMint = [(Type, Type)] -- (currency, datum)

type TransactionOutputs = [(Type, Type)] -- (currency, datum)

type TransactionInputs = ((Type, Type, Type) -> Type) -> Type

class FamilyTransaction (family :: TransactionKind) (transaction :: TransactionKind) where
  transactionFamily :: transaction -> family
  familyTransaction :: family -> Maybe transaction
  -- familyTransaction (transactionFamily t) == Just t

newtype TokenAmounts _redeemer = TokenAmounts (Map TokenName Positive)

data UTxO currencies datum redeemer = UTxO (Address datum redeemer) (currencies TokenAmounts) datum

data RedeemedUTxO currencies datum redeemer = RedeemedUTxO (UTxO currencies datum redeemer) redeemer

data Address datum redeemer =
  ValidatorScriptAddress (datum -> redeemer -> ScriptContext -> Bool)
  | PublicAddress PubKeyHash

newtype MintingPolicy redeemer = MintingPolicy (redeemer -> ScriptContext -> Bool)

data TxInfo currencies inputs outputs = TxInfo
  { txInfoInputs :: inputs (RedeemedUTxO currencies)
  , txInfoOutputs :: outputs (UTxO currencies)
  , txInfoFee :: Value
  , txInfoMint :: currencies Amount
  , txInfoDCert :: [DCert]
  , txInfoWdrl :: [(StakingCredential, Integer)]
  , txInfoValidRange :: POSIXTimeRange
  , txInfoSignatories :: [PubKeyHash]
  , txInfoData :: [(DatumHash, Datum)] 
  , txInfoId :: TxId
  }

testTransaction :: inputs (RedeemedUTxO currencies) -> outputs (UTxO currencies) -> currencies MintingPolicy
                -> POSIXTimeRange -> [PubKeyHash] -> [(DatumHash, Datum)] -> Bool
testTransaction inputs outputs mintingPolicies times signatures dataMap =
~~~

From the inside perspective of a single on-chain script, there's a transaction
to validate and a redeemer. A minting policy script also gets to know its
`ownCurrencySymbol` and for a validator script one of the transaction's inputs
is marked as the validator's own input.

Scripts matter because they have addresses.

One of the major problems with verifying complex distributed apps on Cardano
occurs when its transactions consume multiple inputs locked by multiple
different scripts.

* From the designer's perspective, the transaction is valid if it satisfies
  certain conditions: it doesn't matter which script checks those conditions.

* From the script developer's perspective, when triggered by a transaction the
  script needs to check its own particular subset of those conditions. The
  transaction is valid if every script it triggers says it's valid.

The problem then is, how to split up the validity conditions among the scripts
so that every condition is checked by some script, and ideally only once? One
major issue is that few projects even provide an overview of all possible
transactions.
