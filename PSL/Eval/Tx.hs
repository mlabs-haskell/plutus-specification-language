module PSL.Eval.Tx (
  Value (..),
  PubKeyHash (..),
  CurrencySymbol (..),
  TokenName (..),
  Address (..),
  Data (..),
  UTXORef (..),
  prettyDict,
  prettyCon,
) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Prettyprinter (Doc, Pretty (pretty))
import Prettyprinter qualified as P

data Value = Value
  { valueAda :: Integer
  , valueOwn :: Map TokenName Integer
  , valueOther :: Map CurrencySymbol (Map TokenName Integer)
  }
  deriving stock (Eq, Ord)

instance Semigroup Value where
  Value a o xs <> Value a' o' ys =
    Value
      (a + a')
      (Map.filter (/= 0) $ Map.unionWith (+) o o')
      ( Map.filter Map.null $
          Map.map (Map.filter (/= 0)) $
            Map.unionWith (Map.unionWith (+)) xs ys
      )

instance Monoid Value where
  mempty = Value 0 Map.empty Map.empty

prettyCon :: Doc a -> [Doc a] -> Doc a
prettyCon con args = P.group . P.hang 2 . P.parens $ con <> P.line <> P.sep args

-- | @{k1: v1, k2: v2, ...}@
prettyDict :: [(Doc a, Doc a)] -> Doc a
prettyDict =
  P.group
    . P.encloseSep (P.flatAlt "{ " "{") (P.flatAlt " }" "}") ", "
    . fmap (\(k, v) -> P.group $ P.hang 2 (k <> ":" <> P.line <> v))

{-
\${ ADA: n
  , OWN: {$(t1): n, $(t2): n, ...}
  , cs: {$(t1): n, $(t2): n, ...}
  , ...
  }
-}
instance Pretty Value where
  pretty (Value a os xss) =
    let pmap = prettyDict . fmap (bimap pretty pretty)
        other = fmap (bimap pretty (pmap . Map.toList)) . Map.toList $ xss
        own = [("OWN", pmap . Map.toList $ os) | not $ Map.null os]
        ada = [("ADA", pretty a) | a /= 0]
     in "$" <> prettyDict (ada <> own <> other)

newtype PubKeyHash = PubKeyHash {unPubKeyHash :: ByteString}
  deriving stock (Eq, Ord)

-- PK#123abcf
instance Pretty PubKeyHash where
  pretty (PubKeyHash x) = "PK#" <> prettyHex x

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: ByteString}
  deriving stock (Eq, Ord)

-- CS#123abcf
instance Pretty CurrencySymbol where
  pretty (CurrencySymbol x) = "CS#" <> prettyHex x

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving stock (Eq, Ord)

-- \$(tok name)
instance Pretty TokenName where
  pretty (TokenName x) = "$" <> P.parens (pretty $ T.unpack $ T.decodeUtf8 x)

newtype Address = AddrPubKey PubKeyHash
  deriving stock (Eq, Ord)

instance Pretty Address where
  pretty (AddrPubKey pkh) = pretty pkh

data Data
  = DataConstr Integer [Data]
  | DataMap [(Data, Data)]
  | DataList [Data]
  | DataInt Integer
  | DataBS ByteString
  deriving stock (Eq, Ord)

instance Pretty Data where
  pretty = \case
    DataConstr n x -> prettyCon ("@" <> P.parens (pretty n)) [pretty x] -- (@(1) v)
    DataMap xs -> "@" <> prettyDict (fmap (bimap pretty pretty) xs) -- @{k: v, ...}
    DataList xs -> "@" <> pretty xs -- @[a, b, ...]
    DataInt n -> "@" <> pretty n -- @123
    DataBS bs -> "@" <> P.dquotes (pretty $ T.unpack $ T.decodeUtf8 bs) -- @"abc"

newtype UTXORef = UTXORef {unUTXORef :: ByteString}
  deriving stock (Eq, Ord)

-- UTXO#123abcf
instance Pretty UTXORef where
  pretty (UTXORef x) = "UTXO#" <> prettyHex x

toHex :: ByteString -> String
toHex bs = do
  x <- BS.unpack bs
  fmap (intToDigit . fromIntegral) [x `div` 16, x `mod` 16]

prettyHex :: ByteString -> Doc a
prettyHex bs = pretty $ take 7 $ toHex bs
