{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Spec where

import Data.Kind (Type)
import Family
import GHC.TypeLits (Symbol)
import Numeric.Natural (Natural)

data TransactionFamily
  = MyTransaction Symbol Natural

data MyDApp
  = MyMintingPolicy
  | MyValidator

type instance DApp (t :: TransactionFamily) = MyDApp

type instance Economy MyDApp = Token

data Token
  = Ada
  | KnownToken
  | OwnMinted MyToken

data MyToken = MyMintedToken

-- * Minting policies

instance MintingPolicyScript 'MyMintingPolicy where
  type MintedToken 'MyMintingPolicy = MyToken
  type MintRedeemer 'MyMintingPolicy = ()

-- * Validators

data MyValidatorDatum = MyDatum

data MyValidatorRedeemer = MyRedeemer

instance ValidatorScript 'MyValidator where
  type Datum 'MyValidator = MyValidatorDatum
  type Redeemer 'MyValidator = MyValidatorRedeemer

-- * Transactions

type MyTxInputs :: Symbol -> InputsFor MyDApp
data MyTxInputs user script wallet = MyTxInputs
  { script :: script 'MyValidator ('Just 'MyRedeemer) 'MyDatum '[ 'AnythingElse ],
    payment :: wallet user '[ 'Some 'Ada ]
  }

type MyTxMints :: Natural -> MintsFor MyDApp
data MyTxMints quantity mp = MyTxMints
  { newToken :: mp 'MyMintingPolicy '() '[ 'Mint quantity 'MyMintedToken ]
  }

type MyTxOutputs :: Symbol -> Natural -> OutputsFor MyDApp
data MyTxOutputs user quantity script wallet = MyTxOutputs
  { script :: script 'MyValidator 'MyDatum '[ 'Some 'Ada, 'AnythingElse ],
    purchase :: wallet user '[ 'Exactly quantity ('OwnMinted 'MyMintedToken) ]
  }

instance Transaction (MyTransaction user quantity) where
  type Inputs (MyTransaction user quantity) = MyTxInputs user
  type Mints (MyTransaction user quantity) = MyTxMints quantity
  type Outputs (MyTransaction user quantity) = MyTxOutputs user quantity

main = putStrLn "Compiles."
