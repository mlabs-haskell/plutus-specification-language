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
data Token = Ada | Token Natural | Token :+ Token

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer = Trade | Update

class ValidatorScript s where
  type Currency s :: k
  type Datum s    :: Type
  type Redeemer s :: Type
class Transaction (t :: familie) where
  type Inputs t  :: [Type]
  type Outputs t :: [Type]

instance ValidatorScript ('Oracle n) where
  type Currency ('Oracle n) = 'Token n
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()

instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = '[ScriptInput ('Oracle n) 'Update]
  type Outputs ('UpdateOracle n) = '[ScriptOutput ('Oracle n)]
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = [
    ScriptInput 'CentralExchange '(),
    ScriptInput ('Oracle m) 'Trade,
    ScriptInput ('Oracle n) 'Trade,
    WalletInput ('Token m :+ 'Ada),
    WalletInput ('Token n :+ 'Ada)]
  type Outputs ('Exchange m n) = [
    ScriptOutput 'CentralExchange,
    ScriptOutput ('Oracle m),
    ScriptOutput ('Oracle n),
    WalletOutput ('Token m),
    WalletOutput ('Token n)]
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = '[ScriptInput 'CentralExchange '()]
  type Outputs 'DrainCollectedFees = '[ScriptOutput 'CentralExchange]
~~~

The above declarations are already something that could be automatically
converted into a diagram. They also have a side benefit of being valid Haskell
declarations that can be verified and built upon.

The instance declarations depend on the following definitions, presented here
with kind signatures restricted to the example transaction family but
otherwise reusable across different transaction families and DApps:

~~~ {.haskell.ignore}
class ValidatorScript (script :: DApp) where
  type Currency script :: Token
  type Datum script :: Type
  type Redeemer script :: Type

class Transaction (t :: TransactionFamily) where
  type Inputs t :: [Type]
  type Mints t :: [Type]
  type Outputs t :: [Type]
  type Mints t = '[]
~~~

Note the dependent kind quantification here, necessary because the redeemer
type depends on the script:

~~~ {.haskell}
type ScriptInput :: forall (script :: DApp) -> Redeemer script -> Type
data ScriptInput script redeemer
data ScriptOutput (script :: DApp)

data WalletInput currency
data WalletOutput currency
~~~

Unfortunately GHC's kinds other than `Type` turn out to be difficult to work with, so we'll replace the use of list
kinds with [Higher-Kinded Data](HKD.md).
