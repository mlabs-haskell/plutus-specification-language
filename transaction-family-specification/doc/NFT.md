## Tokenized transaction families

<!--
~~~ {.haskell}
{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, StandaloneKindSignatures,
             PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module NFT where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Numeric.Natural (Natural)
import Family

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy ExchangeDApp = Token
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

type InitializeInputs :: InputsFor ExchangeDApp
data InitializeInputs s w = InitializeInputs {
  nftSource :: w "Owner" 'Nothing '[]}
type InitializeMints :: MintsFor ExchangeDApp
data InitializeMints mp = InitializeMints {
  authorization :: mp 'AuthorizingMint '() '[ 'Mint 1 'AuthNFT ]}
type InitializeOutputs :: OutputsFor ExchangeDApp
data InitializeOutputs s w = InitializeOutputs {
  exchange :: s 'CentralExchange '() '[ 'Exactly 1 ('Minted 'AuthNFT) ]}
instance Transaction 'Initialize where
  type Inputs 'Initialize = InitializeInputs
  type Mints 'Initialize = InitializeMints
  type Outputs 'Initialize = InitializeOutputs
~~~
