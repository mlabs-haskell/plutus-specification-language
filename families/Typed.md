## Typing transaction families

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, PolyKinds, TypeFamilies, TypeOperators #-}

module Typed where

import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)

data POSIXTime

~~~
-->

The [preceding concepts](TransactionFamily.md) were relying on transactions,
UTxOs, and scripts without defining them. They could be built on top of the
actual live transactions on the blockchain. But that's not what we're after
here, we're interested in abstracted transactions.

Here are some properties we can safely abstract away:

* Actual numerical or string values of script hashes and addresses. We're only
  interested in their identities, which can be represented with identifiers.
* The quantities inside values, for the most part: we do care what token types
  were involved, how many of each type rarely matters. More precisely, we may
  want a static assurance that there is:
  - no given token,
  - exactly one token,
  - zero or one token, or
  - an arbitrary positive amount of tokens.
* The values of UTxO datums and redeemers; we're only interested in their types.
* Transaction signatories, certificates, datum hashes, and similar proofs can
  just be assumed to be present.
* Transaction validity range.

## Example

Imagine a DApp that consists of N+1 scripts: N being oracles that maintain
the going price of different tokens and one an exchange that gets its prices
from the oracles.

* Each oracle can be updated by its authority, providing for N different
  transaction types that involve only that oracle.
* An exchange transaction involves the exchange script, two oracles to look
  up the prices, and one or two wallets.
* A percentage of every exchange gets collected as a fee, which can be
  drained by the exchange authority in a separate transaction.

Once abstracted of specific amounts and other details, all different
transactions listed above make up a transaction family of N²+N+1 transaction
types. We can represent these types in Haskell as follows.

~~~ {.haskell}
data DApp = Oracle Natural | CentralExchange
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
data Token = Ada | Token Natural

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer = Trade | Update

instance ValidatorScript ('Oracle n) where
  type Currency ('Oracle n) = 'Token n
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()

instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = '[Input ('Oracle n) 'Update]
  type Outputs ('UpdateOracle n) = '[Output ('Oracle n)]
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = [
    Input 'CentralExchange '(),
    Input ('Oracle m) 'Trade,
    Input ('Oracle n) 'Trade,
    WalletInput ('Token m :+ 'Ada),
    WalletInput ('Token n :+ 'Ada)]
  type Outputs ('Exchange m n) = [
    Output 'CentralExchange,
    Output ('Oracle m),
    Output ('Oracle n),
    WalletOutput ('Token m),
    WalletOutput ('Token n)]
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = '[Input 'CentralExchange '()]
  type Outputs 'DrainCollectedFees = '[Output 'CentralExchange]
~~~

The instance declarations depend on the following definitions that are tied to
the example transaction family, but can be generalized to become reusable across
different transaction families and DApps:

~~~ {.haskell}
class ValidatorScript (script :: DApp) where
  type Currency script :: Token
  type Datum script :: Type
  type Redeemer script :: Type

class Transaction (t :: TransactionFamily) where
  type Inputs t :: [Type]
  type Mints t :: [Type]
  type Outputs t :: [Type]
  type Mints t = '[]

type Input :: forall (script :: DApp) -> Redeemer script -> Type
data Input script redeemer = Input
data Output (script :: DApp)

data WalletInput currency
data WalletOutput currency

data c1 :+ c2
~~~

Is this transaction family open or closed? A most likely design would have the
oracles open to other exchanges and other kinds of dApps, so at least the
`Trade` redeemer would be accessible to anyone. However there is no value in any
`Value` locked by an oracle, so this opening is not much of a vulnerability
itself. We just need to ensure that every oracle verifies that its
`priceInLovelace` cannot be changed by any transaction using this redeemer.

The `CentralExchange` script on the other hand would likely be parameterized
by a whitelist of accepted oracles. Unfortunately the same method can't be
applied in the opposite direction: if we tried to *also* supply the
`CentralExchange` script address as a parameter to every oracle there'd be a
cyclic dependency. We can't close the type family this way.

One workaround is to rely on an NFT whose sole token is carried by our
`CentralExchange`. The NFT identity can be supplied as a parameter to every
oracle as well as the exchange, so there's no cyclic dependency. With this
addition we can close our dApp so it looks as follows:


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
