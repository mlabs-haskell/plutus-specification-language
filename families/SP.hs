{-# LANGUAGE DataKinds, DuplicateRecordFields, GADTs, FlexibleInstances, OverloadedStrings,
             KindSignatures, StandaloneKindSignatures,
             MultiParamTypeClasses, NoStarIsType, NumericUnderscores,
             PolyKinds, RankNTypes, TypeApplications, TypeFamilies, TypeOperators #-}

module SP where

import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Numeric.Natural (Natural)

import Families
import Ledger (POSIXTime (POSIXTime), PubKey, always)

data TransactionFamily =
  Create CreateParam
  | Deposit DepositParam
  | Withdraw WithdrawParam
  | Close CloseParam

data CreateParam = CreateParam
  { cTokenName :: TokenName,
    cAmount :: Integer
  }
data DepositParam = DepositParam
  { dTokenName :: TokenName,
    dAmount :: Integer
  }
data WithdrawParam = WithdrawParam
  { wTokenName :: TokenName,
    wAmount :: Integer
  }
newtype CloseParam = CloseParam
  { clTokenName :: TokenName
  }

data StabilityPoolRedeemer
  = CreateAccount {caPkh :: Ledger.PaymentPubKeyHash, caAmount :: Integer}
  | AdjustAccount {aaChangedAmount :: Integer}
  | LiquidateCDP
  | Close
  | SpendAccount
  | UpgradeVersion

data StabilityDatum
  = StabilityPoolDatum
      { spIAsset :: TokenName,
        spSnapshot :: StabilityPoolSnapshot,
        epochToScaleToSum :: EpochToScaleToSum
      }
  | AccountDatum
      { accOwner :: Ledger.PaymentPubKeyHash,
        accIAsset :: TokenName,
        accSnapshot :: StabilityPoolSnapshot
      }

data CDPDatum
  = CDPDatum
      { cdpOwner :: Ledger.PaymentPubKeyHash,
        -- | Name of iAsset that can be minted from this position.
        cdpIAsset :: TokenName,
        cdpMintedAmount :: Integer
      }
  | IAssetDatum IAsset
data IAsset = IAsset
  { iaName :: Value.TokenName,
    -- | The collateral ratio of the IAsset
    iaMinRatio :: OnChainDecimal,
    -- | The Left value is used here only when the oracle has been delisted, otherwise the Right is used to reference the oracle.
    iaPrice :: Either OnChainDecimal (OracleAssetNFT, ValidatorHash)
  }

data CDPRedeemer
  = AdjustCDP
  | CloseCDP
  | Liquidate
  | View
  | UpgradeAsset
  | UpgradeVersion

data CDPCreatorRedeemer = CreateCDP Ledger.PaymentPubKeyHash Integer Integer

data IndigoDApp = StabilityPool Natural | CDP Natural | CDPCreator Natural
data Token = Ada
           | StabilityPoolToken Natural | SPAccountToken Natural
           | IAsset Natural | IAssetToken Natural
           | CDPCreatorNft | CDPAuthToken

instance ValidatorScript ('StabilityPool n) where
  type Currencies ('StabilityPool n) = [ 'StabilityPoolToken n, 'IAsset n,
                                         'SPAccountToken 1, 'SPAccountToken 2 ]
  type Datum ('StabilityPool n) = StabilityPoolDatum
  type Redeemer ('StabilityPool n) = StabilityPoolRedeemer
instance ValidatorScript ('CDP n) where
  type Currencies ('CDP n) = [ 'CDPAuthToken, 'IAssetToken n, 'IAsset n,
                               'SPAccountToken 1, 'SPAccountToken 2 ]
  type Datum ('CDP n) = CDPDatum
  type Redeemer ('CDP n) = CDPRedeemer
instance ValidatorScript ('CDP n) where
  type Currencies ('CDP n) = [ 'CDPAuthToken, 'IAsset n,
                               'SPAccountToken 1, 'SPAccountToken 2 ]
  type Datum ('CDP n) = CDPDatum
  type Redeemer ('CDP n) = CDPRedeemer
instance ValidatorScript ('CDPCreator n) where
  type Currencies ('CDPCreator n) = '[ 'CDPCreatorNft ]
  type Datum ('CDPCreator n) = ()
  type Redeemer ('CDPCreator n) = CDPCreatorRedeemer

type instance DApp (t :: TransactionFamily) = ExchangeDApp
type instance Economy (t :: TransactionFamily) = Token

type UpdateOracleInputs :: Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> (c -> Type) -> Type
data UpdateOracleInputs n s w = UpdateOracleInputs {
  oracle :: s ('StabilityPool n) 'Update}
type UpdateOracleOutputs :: Natural -> (ExchangeDApp -> Type) -> (c -> Type) -> Type
data UpdateOracleOutputs n s w = UpdateOracleOutputs {
  oracle :: s ('StabilityPool n)}
instance Transaction ('UpdateOracle n) where
  type Inputs ('UpdateOracle n) = UpdateOracleInputs n
  type Outputs ('UpdateOracle n) = UpdateOracleOutputs n

type ExchangeInputs :: Natural -> Natural -> (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> ([Token] -> Type) -> Type
data ExchangeInputs m n s w = ExchangeInputs {
  exchange :: s 'CentralExchange '(),
  oracle1 :: s ('StabilityPool m) 'Trade,
  oracle2 :: s ('StabilityPool n) 'Trade,
  wallet1 :: w '[ 'Token m ],
  wallet2 :: w ' ['Token n ]}
data ExchangeOutputs m n s w = ExchangeOutputs {
  exchange :: s 'CentralExchange,
  oracle1 :: s ('StabilityPool m),
  oracle2 :: s ('StabilityPool n),
  wallet1 :: w '[ 'Token n ],
  wallet2 :: w '[ 'Token m ]}
instance Transaction ('Exchange m n) where
  type Inputs ('Exchange m n) = ExchangeInputs m n
  type Outputs ('Exchange m n) = ExchangeOutputs m n

type DrainInputs :: (forall (s :: ExchangeDApp) -> Redeemer s -> Type) -> ([Token] -> Type) -> Type
data DrainInputs s w = DrainInputs {
  echange :: s 'CentralExchange '()}
data DrainOutputs s w = DrainOutputs {
  echange :: s 'CentralExchange}
instance Transaction 'DrainCollectedFees where
  type Inputs 'DrainCollectedFees = DrainInputs
  type Outputs 'DrainCollectedFees = DrainOutputs

