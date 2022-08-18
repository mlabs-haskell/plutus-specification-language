## Tokenized transaction families


<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module NFT where

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Family

data POSIXTime
~~~
-->

~~~ {.haskell}
data MyDApp = Oracle Natural | CentralExchange
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
  | Initialize
data Token = Token Natural | AuthNFT | ScriptAda

data OracleDatum = OracleDatum {
  priceInLovelace :: Natural,
  maxTradeVolume :: Natural,
  expiry :: POSIXTime
  }
data OracleRedeemer = Trade | Update

instance ValidatorScript CentralExchange where
  type Currencies CentralExchange = ['ScriptAda, 'AuthNFT]
  type Datum CentralExchange = ()
  type Redeemer CentralExchange = ()

type instance DApp 'Initialize = MyDApp
type instance Economy 'Initialize = Token

type InititalizeOutputs :: (forall (s :: MyDApp) -> Datum s -> [Token] -> Type) -> (c -> Type) -> Type
data InititalizeOutputs s w = InititalizeOutputs {
  exchange :: s 'CentralExchange '() '[ 'AuthNFT ]}
instance Transaction 'Initialize where
  type Inputs 'Initialize = InputWallet '[ 'ScriptAda ]
  type Outputs 'Initialize = InititalizeOutputs
~~~
