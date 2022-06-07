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
instance ValidatorScript ('Oracle n) where
  type Currency ('Oracle n) = 'Token n
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer
instance ValidatorScript CentralExchange where
  type Currency CentralExchange = 'Ada :+ 'AuthNFT
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
