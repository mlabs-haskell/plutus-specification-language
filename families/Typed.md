## Typing transaction families

So we have added some superstructure on top of Cardano concepts, but how does
it help?

<!--
~~~ {.haskell}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses #-}

module Typed where

import Data.Kind (Type)
import Data.Map (Map)
~~~
-->

~~~ {.haskell}

data Amount :: Type -> Type
data Datum
data DatumHash
data DCert
data Positive
data POSIXTimeRange
data PubKeyHash
data ScriptContext
data StakingCredential
data TokenName
data TxId
data Value

type TransactionKind = (TransactionInputs, TransactionMint, TransactionOutputs)

type TransactionInputs = [(Type, Type, Type)] -- (currency, datum, redeemer)

type TransactionMint = [(Type, Type)] -- (currency, datum)

type TransactionOutputs = [(Type, Type)] -- (currency, datum)

-- type TransactionInputs = ((Type, Type, Type) -> Type) -> Type

class FamilyTransaction famille transaction where
  transactionFamily :: transaction -> famille
  familyTransaction :: famille -> Maybe transaction
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
testTransaction inputs outputs mintingPolicies times signatures dataMap = undefined
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
