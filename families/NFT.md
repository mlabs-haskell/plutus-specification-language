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
import Ledger
import Family

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy (t :: TransactionFamily) = Token
~~~
-->

First we expand the list of `DApp` scripts with a minting policy script `AuthorizingMint`, and the transaction family
with the `Initialize` transaction:

~~~ {.haskell}
data ExchangeDApp = Oracle Natural | CentralExchange | AuthorizingMint
data TransactionFamily =
  UpdateOracle Natural
  | Exchange Natural Natural
  | DrainCollectedFees
  | Initialize
~~~

The new NFT token needs to be declared separately so we can use it as an associated type that `AuthorizingMint` will
produce:

~~~ {.haskell}
data Token = Token Natural | Minted AuthToken | ScriptAda
data AuthToken = AuthNFT

instance MintingPolicyScript 'AuthorizingMint where
  type MintedToken 'AuthorizingMint = AuthToken
  type MintRedeemer 'AuthorizingMint = ()
~~~

Finally we can plug in the minting policy into the new `Initialize` transaction, which is going to send the minted
`AuthNFT` to the `CentralExchange` script address.

~~~ {.haskell}
instance ValidatorScript 'CentralExchange where
  type Currencies 'CentralExchange = ['ScriptAda, 'Minted 'AuthNFT]
  type Datum 'CentralExchange = ()
  type Redeemer 'CentralExchange = ()

type InitializeMints :: (forall (mp :: ExchangeDApp) -> MintRedeemer mp -> [MintedToken mp] -> Type) -> Type
data InitializeMints mp = InitializeMints {
  authorization :: mp 'AuthorizingMint '() '[ 'AuthNFT ]}
type InititalizeOutputs :: (forall (s :: ExchangeDApp) -> Datum s -> [Token] -> Type) -> (c -> Type) -> Type
data InititalizeOutputs s w = InititalizeOutputs {
  exchange :: s 'CentralExchange '() '[ 'Minted 'AuthNFT ]}
instance Transaction 'Initialize where
  type Inputs 'Initialize = InputWallet '[ 'ScriptAda ]
  type Mints 'Initialize = InitializeMints
  type Outputs 'Initialize = InititalizeOutputs
~~~
