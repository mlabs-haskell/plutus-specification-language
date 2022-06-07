## Tokenized transaction families


<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, PolyKinds, TypeFamilies, TypeOperators #-}

module NFT where

import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Families

data POSIXTime
~~~
-->

~~~ {.haskell}
data DApp = Oracle Natural | CentralExchange
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
  | Initialize
data Token = Ada | Token Natural | AuthNFT | Token :+ Token

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer = Trade | Update
~~~

<!--
~~~ {.haskell}
type Input :: forall (script :: DApp) -> Redeemer script -> Type
data Input script redeemer = Input
data Output (script :: DApp)

data WalletInput currency
data WalletOutput currency
~~~
-->

~~~ {.haskell}
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada :+ 'AuthNFT
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()

instance Transaction 'Initialize where
  type Inputs 'Initialize = '[WalletInput 'Ada]
  type Outputs 'Initialize = '[Output 'CentralExchange]
~~~
