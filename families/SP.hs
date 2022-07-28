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
import Ledger (POSIXTime (POSIXTime), PubKey, AssetClass, TokenName, ValidatorHash, always)
import qualified Ledger

data TransactionFamily =
  StartOracle StartOracleParam
  | FeedOraclePrice Natural --FeedPriceParam
  | Create CreateParam
  | Deposit DepositParam
  | Withdraw WithdrawParam
  | Close CloseParam

data IndigoDApp = Oracle Natural | CDP Natural | StabilityPool Natural | CDPCreator Natural

data Token = Ada
           | OracleAssetNFToken Natural
           | StabilityPoolToken Natural | SPAccountToken Natural
           | IAssetAmount Natural | IAssetToken Natural
           | CDPCreatorNft | CDPAuthToken

type instance DApp (t :: TransactionFamily) = IndigoDApp
type instance Economy (t :: TransactionFamily) = Token

data StartOracleParam = StartOracleParam
  { sopAssetToken :: TokenName,
    sopAssetPrice :: OnChainDecimal,
    sopOracleParams :: OracleParams
  }

data FeedPriceParam = FeedPriceParam
  { fpOracle :: OracleParams,
    fpIAssetNFT :: OracleAssetNFT,
    fpPrice :: OnChainDecimal
  }

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

data OracleDatum = MkOracleDatum
  { -- | Price in lovelace with decimal places
    odPrice :: OnChainDecimal,
    odExpiration :: POSIXTime
  }

data OracleRedeemer (n :: Natural) = Use | FeedPrice -- POSIXTime OnChainDecimal

data StabilityPoolRedeemer (n :: Natural)
  = CreateAccount {caPkh :: Ledger.PaymentPubKeyHash, caAmount :: Integer}
  | AdjustAccount {aaChangedAmount :: Integer}
  | LiquidateCDP
  | CloseSP
  | SpendAccount
--  | UpgradeVersion

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
  { iaName :: TokenName,
    -- | The collateral ratio of the IAsset
    iaMinRatio :: OnChainDecimal,
    -- | The Left value is used here only when the oracle has been delisted, otherwise the Right is used to reference the oracle.
    iaPrice :: Either OnChainDecimal (OracleAssetNFT, ValidatorHash)
  }

data CDPRedeemer (n :: Natural)
  = AdjustCDP
  | CloseCDP
  | Liquidate
  | View
  | UpgradeAsset
  | UpgradeVersion

data CDPCreatorRedeemer (n :: Natural) = CreateCDP Ledger.PaymentPubKeyHash Integer Integer

data OracleParams = OracleParams
  { -- | Owner of the Oracle - the PubKeyHash that can update the authentic Oracle output with NFT
    opOwner :: Ledger.PaymentPubKeyHash,
    -- | the time in which time dependend transactions should finish (e.g. feedOracle, useOracle)
    opTimeBuffer :: POSIXTime,
    -- | the time how long the oracle is active
    opExpirationTime :: POSIXTime
  }

newtype OracleAssetNFT = MkOracleAssetNFT AssetClass

data StabilityPoolSnapshot = StabilityPoolSnapshot
  { snapshotP :: OnChainDecimal,
    snapshotD :: OnChainDecimal,
    snapshotS :: OnChainDecimal,
    snapshotEpoch :: Integer,
    snapshotScale :: Integer
  }

type EpochToScaleToSum = Map (Integer, Integer) OnChainDecimal

newtype OnChainDecimal = OnChainDecimal {getOnChainInt :: Integer}

instance ValidatorScript ('Oracle n) where
  type Currencies ('Oracle n) = '[ 'OracleAssetNFToken n ]
  type Datum ('Oracle n) = OracleDatum
  type Redeemer ('Oracle n) = OracleRedeemer n
instance ValidatorScript ('StabilityPool n) where
  type Currencies ('StabilityPool n) = [ 'StabilityPoolToken n, 'IAssetAmount n,
                                         'SPAccountToken 1, 'SPAccountToken 2 ]
  type Datum ('StabilityPool n) = StabilityDatum
  type Redeemer ('StabilityPool n) = StabilityPoolRedeemer n
instance ValidatorScript ('CDP n) where
  type Currencies ('CDP n) = [ 'CDPAuthToken, 'IAssetToken n, 'IAssetAmount n,
                               'SPAccountToken 1, 'SPAccountToken 2 ]
  type Datum ('CDP n) = CDPDatum
  type Redeemer ('CDP n) = CDPRedeemer n
instance ValidatorScript ('CDPCreator n) where
  type Currencies ('CDPCreator n) = '[ 'CDPCreatorNft ]
  type Datum ('CDPCreator n) = ()
  type Redeemer ('CDPCreator n) = CDPCreatorRedeemer n

type FeedOraclePriceInputs :: Natural -> (forall (s :: IndigoDApp) -> Redeemer s -> [Token] -> Type) -> (c -> Type) -> Type
data FeedOraclePriceInputs n s w = FeedOraclePriceInputs {
  oracle :: s ('Oracle n) 'FeedPrice [ 'OracleAssetNFToken n, 'IAssetAmount n ]}
type FeedOraclePriceOutputs :: Natural -> (IndigoDApp -> [Token] -> Type) -> (c -> Type) -> Type
data FeedOraclePriceOutputs n s w = FeedOraclePriceOutputs {
  oracle :: s ('Oracle n) '[]}

instance Transaction ('FeedOraclePrice n) where
  type Inputs ('FeedOraclePrice n) = FeedOraclePriceInputs n
  type Outputs ('FeedOraclePrice n) = FeedOraclePriceOutputs n
