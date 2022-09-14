{-# Language DataKinds, RankNTypes, StandaloneKindSignatures, TypeFamilies #-}

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
  type Currencies 'ValidatorScript = ['MintedToken 'MPToken, 'OtherToken]
  type Datum 'ValidatorScript = ValidatorDatum
  type Redeemer 'ValidatorScript = ValidatorRedeemer

instance Family.MintingPolicyScript 'MintingPolicy where
  type MintedToken 'MintingPolicy = MPToken
  type MintRedeemer 'MintingPolicy = PolicyRedeemer

type TransactionInputs :: Family.InputsFor DApp
data TransactionInputs s w = TransactionInputs {
  scriptReferenceInput :: s 'ValidatorScript 'Nothing 'ValidatorDatum '[ 'MintedToken 'MPToken, 'OtherToken ],
  scriptConsumedInput :: s 'ValidatorScript ('Just 'ValidatorRedeemer) 'ValidatorDatum '[ 'MintedToken 'MPToken, 'OtherToken ],
  walletInput :: w "Wallet" '[ 'MintedToken 'MPToken ]}
type TransactionMints :: Family.MintsFor DApp
data TransactionMints mp = TransactionMints {
  mintedTokens :: mp 'MintingPolicy 'PolicyRedeemer '[ 'Family.Mint 1 'MPToken ]}
type TransactionOutputs :: Family.OutputsFor DApp
data TransactionOutputs s w = TransactionOutputs {
  scriptOutput :: s 'ValidatorScript 'ValidatorDatum '[ 'MintedToken 'MPToken, 'OtherToken ],
  walletOutput :: w "Wallet" '[ 'OtherToken ]}
instance Family.Transaction 'Transaction where
  type Inputs 'Transaction = TransactionInputs
  type Mints 'Transaction = TransactionMints
  type Outputs 'Transaction = TransactionOutputs
