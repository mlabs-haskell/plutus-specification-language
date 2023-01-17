{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Family.Ledger where

import Data.ByteString (ByteString)
import Data.String (IsString)

newtype PubKey = PubKey {getPubKey :: LedgerBytes} deriving (Eq, Ord, IsString, Show)

newtype PubKeyHash = PubKeyHash {getPubKeyHash :: LedgerBytes} deriving (Eq, Ord, IsString, Show)

newtype PaymentPubKeyHash = PaymentPubKeyHash {getPaymentPubKeyHash :: LedgerBytes} deriving (Eq, IsString, Show)

newtype ValidatorHash = ValidatorHash {getValidatorHash :: LedgerBytes} deriving (Eq, Ord, IsString, Show)

newtype LedgerBytes = LedgerBytes {getLedgerBytes :: BuiltinByteString} deriving (Eq, Ord, IsString, Show)

newtype BuiltinByteString = BuiltinByteString ByteString deriving (Eq, Ord, IsString, Show)

newtype Signature = Signature {getSignature :: BuiltinByteString} deriving (Eq, IsString, Show)

newtype POSIXTime = POSIXTime {getPOSIXTime :: Integer} deriving (Eq, Ord, Num)

newtype Slot = Slot {getSlot :: Integer} deriving (Eq, Ord, Num)

type SlotRange = Interval Slot

type POSIXTimeRane = Interval POSIXTime

data Interval a

always :: Interval a
always = undefined

newtype Value = Value {getValue :: Map CurrencySymbol (Map TokenName Integer)}

newtype Map k v = Map {unMap :: [(k, v)]}

data AssetClass

data CurrencySymbol

data TokenName
