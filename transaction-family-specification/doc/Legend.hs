{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Legend where

import qualified Family

data DApp = ValidatorScript | MintingPolicy

data Family = Transaction

data ValidatorRedeemer = ValidatorRedeemer

data ValidatorDatum = ValidatorDatum

data PolicyRedeemer = PolicyRedeemer

data Token = MintedToken MPToken | OtherToken

data MPToken = MPToken

type instance Family.Economy DApp = Token

type instance Family.DApp (t :: Family) = DApp

instance Family.ValidatorScript 'ValidatorScript where
  type Datum 'ValidatorScript = ValidatorDatum
  type Redeemer 'ValidatorScript = ValidatorRedeemer

instance Family.MintingPolicyScript 'MintingPolicy where
  type MintedToken 'MintingPolicy = MPToken
  type MintRedeemer 'MintingPolicy = PolicyRedeemer

type TransactionInputs :: Family.InputsFor DApp
data TransactionInputs s w = TransactionInputs
  { scriptReferenceInput ::
      s
        'ValidatorScript
        'Nothing
        'ValidatorDatum
        '[ 'Family.Some ('MintedToken 'MPToken), 'Family.Some 'OtherToken],
    scriptConsumedInput ::
      s
        'ValidatorScript
        ('Just 'ValidatorRedeemer)
        'ValidatorDatum
        '[ 'Family.Some ('MintedToken 'MPToken), 'Family.Some 'OtherToken],
    walletInput :: w "User" '[ 'Family.Some ('MintedToken 'MPToken)]
  }

type TransactionMints :: Family.MintsFor DApp
data TransactionMints mp = TransactionMints
  { mintedTokens :: mp 'MintingPolicy 'PolicyRedeemer '[ 'Family.Mint 1 'MPToken]
  }

type TransactionOutputs :: Family.OutputsFor DApp
data TransactionOutputs s w = TransactionOutputs
  { scriptOutput ::
      s 'ValidatorScript 'ValidatorDatum '[ 'Family.Some ('MintedToken 'MPToken), 'Family.Some 'OtherToken],
    walletOutput :: w "User" '[ 'Family.Some 'OtherToken]
  }

instance Family.Transaction 'Transaction where
  type Inputs 'Transaction = TransactionInputs
  type Mints 'Transaction = TransactionMints
  type Outputs 'Transaction = TransactionOutputs
