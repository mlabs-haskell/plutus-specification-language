{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module exports the templates for automatic instance deriving of a 'TransactionTypeDiagram' value from a
-- transaction type. The most common way to use it would be
--
-- > import qualified Families.Diagram.TH
-- > instance Transaction MyTransaction where ...
-- > myTransactionDiagram = $(Families.Diagram.TH.transactionDiagram ''MyTransaction)
module Family.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType) where

import Control.Applicative (empty)
import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Family (MintQuantity (Burn, BurnSome, Mint, MintOrBurnSome, MintSome), Transaction (Inputs, Mints, Outputs))
import Family.Diagram
  ( Currency (..),
    InputFromScript (..),
    MintOrBurn (..),
    OutputToScript (..),
    TransactionTypeDiagram (..),
    Wallet (..),
  )
import Family.Diagram qualified as D
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Natural)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (liftTyped)

-- | How many items to expend an example list to?
listItems :: Int
listItems = 3

type TypeReifier a = [(TH.Name, Maybe TH.Type)] -> TH.Code TH.Q Text -> TH.Type -> [TH.Code TH.Q (Text, a)]

untypedDiagramForTransactionType :: TH.Type -> TH.Q TH.Exp
untypedDiagramForTransactionType = TH.unTypeCode . diagramForTransactionType

diagramForTransactionType :: TH.Type -> TH.Code TH.Q TransactionTypeDiagram
diagramForTransactionType t = TH.Code $ do
  TH.ClassI _ ins <- TH.reify ''Transaction
  let pred = \case
        TH.InstanceD _ _ t' _ | TH.AppT (TH.ConT ''Transaction) t == t' -> True
        _ -> False
  TH.reifyInstances ''Transaction [t] >>= \case
    [TH.InstanceD {}] -> pure ()
    [] -> TH.reportError ("Missing instance Transaction '" <> TH.pprint t)
    _ -> TH.reportError ("Multiple instances Transaction '" <> TH.pprint t)
  TH.examineCode
    [||
    TransactionTypeDiagram
      $$(textLiteral $ typeDescription [] t)
      (Map.fromList $$(reifyMapField t ''Inputs reifyScriptInput))
      (Map.fromList $$(reifyMapField t ''Outputs reifyScriptOutput))
      (Map.fromList $$(reifyMapField t ''Inputs reifyWallet))
      (Map.fromList $$(reifyMapField t ''Outputs reifyWallet))
      (Map.fromList $$(reifyMapField t ''Mints reifyMint))
    ||]

reifyMapField :: forall a. TH.Type -> TH.Name -> TypeReifier a -> TH.Code TH.Q [(Text, a)]
reifyMapField t assocTypeName reifyType = TH.Code $ do
  let description = TH.pprint assocTypeName <> " '" <> TH.pprint t
      (_, typeArgs) = typeNameAndArgs t
  typeName <-
    TH.reifyInstances assocTypeName [t] >>= \case
      [TH.TySynInstD (TH.TySynEqn _ _ t')] -> pure (fst $ typeNameAndArgs t')
      [d] -> ''Transaction <$ TH.reportError ("Weird instance " <> description <> ": " <> show d)
      [] -> ''Transaction <$ TH.reportError ("Missing instance " <> description)
      _ -> ''Transaction <$ TH.reportError ("Multiple instances " <> description)
  (TH.TyConI tyCon) <- TH.reify typeName
  (tyConName, tyVars, cons) <- case tyCon of
    TH.DataD _ nm tyVars _kind cons _ -> pure (nm, tyVars, cons)
    d -> (typeName, [], []) <$ TH.reportError ("Unexpected declaration of " <> TH.pprint tyCon <> ": " <> show d)
  fields <- case cons of
    [TH.NormalC _ []] -> pure []
    [TH.RecC _ flds] -> pure flds
    [_] -> [] <$ TH.reportError ("Non-record data " <> TH.pprint tyConName <> " declaration")
    [] -> [] <$ TH.reportError ("Empty data " <> TH.pprint tyConName <> " declaration")
    _ -> [] <$ TH.reportError ("Multiple data " <> TH.pprint tyConName <> " constructors")
  let reifyField :: TH.VarBangType -> [TH.Q (TH.TExp (Text, a))]
      reifyField (name, _, fieldType) =
        TH.examineCode <$> reifyType varBindings [||Text.pack $$(stringLiteral $ TH.nameBase name)||] fieldType
      addName :: TH.Name -> TH.TExp a -> TH.Code TH.Q (Text, a)
      addName name a = [||(Text.pack $$(stringLiteral $ TH.nameBase name), $$(TH.Code $ pure a))||]
      varBindings :: [(TH.Name, Maybe TH.Type)]
      varBindings = zip (unKind <$> tyVars) (map Just typeArgs <> repeat Nothing)
  sequenceExps $ sequenceA $ foldMap reifyField fields

reifyScriptInput :: HasCallStack => TypeReifier InputFromScript
reifyScriptInput
  vars
  fieldName
  ( TH.AppT
      ( TH.AppT
          ( TH.AppT
              (TH.AppT (TH.VarT s) scriptType)
              optRedeemerType
            )
          datumType
        )
      currencies
    )
    | _wallet : (scriptVar, _) : _ <- reverse vars,
      s == scriptVar =
        pure
          [||
          ( $$fieldName,
            InputFromScript
              $$(textLiteral $ typeDescription vars scriptType)
              $$(maybeTypeDescriptionQuote vars optRedeemerType)
              $$(textLiteral $ typeDescription vars datumType)
              $$(currencyDescriptionQuotes vars currencies)
          )
          ||]
reifyScriptInput vars fieldName (TH.AppT TH.ListT itemType) = reifyList reifyScriptInput vars fieldName itemType
reifyScriptInput vars fieldName (TH.AppT (TH.AppT (TH.VarT w) name) currencies)
  | (walletVar, _) : _ <- reverse vars, w == walletVar = empty
reifyScriptInput vars _ t = error (Text.unpack (typeDescription vars t) <> ":\n" <> show t)

reifyWallet :: HasCallStack => TypeReifier Wallet
reifyWallet vars fieldName (TH.AppT (TH.AppT (TH.VarT w) name) currencies)
  | (walletVar, _) : _ <- reverse vars,
    w == walletVar =
      pure
        [||
        ($$fieldName,
         Wallet $$(textLiteral $ typeDescription vars name <> "'s wallet") $$(currencyDescriptionQuotes vars currencies))
        ||]
reifyWallet vars fieldName (TH.AppT TH.ListT itemType) = reifyList reifyWallet vars fieldName itemType
reifyWallet
  vars
  fieldName
  ( TH.AppT
      ( TH.AppT
          ( TH.AppT
              (TH.AppT (TH.VarT s) scriptType)
              optRedeemerType
            )
          datumType
        )
      currencies
    )
    | _wallet : (scriptVar, _) : _ <- reverse vars, s == scriptVar = empty
reifyWallet
  vars
  fieldName
  ( TH.AppT
      ( TH.AppT
          (TH.AppT (TH.VarT s) scriptType)
          datumType
        )
      currencies
    )
    | _wallet : (scriptVar, _) : _ <- reverse vars, s == scriptVar = empty
reifyWallet vars _ t = error (Text.unpack (typeDescription vars t) <> ":\n" <> show t)

reifyScriptOutput :: HasCallStack => TypeReifier OutputToScript
reifyScriptOutput
  vars
  fieldName
  ( TH.AppT
      ( TH.AppT
          (TH.AppT (TH.VarT s) scriptType)
          datumType
        )
      currencies
    )
    | _wallet : (scriptVar, _) : _ <- reverse vars,
      s == scriptVar =
        pure
          [||
          ( $$fieldName,
            OutputToScript
              $$(textLiteral $ typeDescription vars scriptType)
              $$(textLiteral $ typeDescription vars datumType)
              $$(currencyDescriptionQuotes vars currencies)
          )
          ||]
reifyScriptOutput vars fieldName (TH.AppT TH.ListT itemType) = reifyList reifyScriptOutput vars fieldName itemType
reifyScriptOutput vars fieldName (TH.AppT (TH.AppT (TH.VarT w) name) currencies)
  | (walletVar, _) : _ <- reverse vars, w == walletVar = empty
reifyScriptOutput vars _ t = error (Text.unpack (typeDescription vars t) <> ":\n" <> show t)

reifyMint :: HasCallStack => TypeReifier MintOrBurn
reifyMint
  vars
  fieldName
  ( TH.AppT
      ( TH.AppT
          (TH.AppT (TH.VarT mp) scriptType)
          redeemerType
        )
      quantities
    )
    | (mpVar, _) : _ <- reverse vars,
      mp == mpVar =
        pure
          [||
          ( $$fieldName,
            MintOrBurn
              $$(textLiteral $ typeDescription vars scriptType)
              $$(textLiteral $ typeDescription vars redeemerType)
              $$(quantitiesDescriptionQuote vars quantities)
          )
          ||]
reifyMint vars fieldName (TH.AppT TH.ListT itemType) = reifyList reifyMint vars fieldName itemType
reifyMint vars _ t = error (Text.unpack (typeDescription vars t) <> ":\n" <> show t)

quantitiesDescriptionQuote ::
  HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> TH.Code TH.Q [MintQuantity Text Currency]
quantitiesDescriptionQuote vars = TH.unsafeCodeCoerce . fmap TH.ListE . mapM TH.unTypeCode . quantityDescriptions vars

quantityDescriptions ::
  HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> [TH.Code TH.Q (MintQuantity Text Currency)]
quantityDescriptions vars (TH.AppT (TH.AppT TH.PromotedConsT t) ts) =
  quantityDescription vars t : quantityDescriptions vars ts
quantityDescriptions vars TH.PromotedNilT = []
quantityDescriptions vars (TH.SigT t _) = quantityDescriptions vars t

quantityDescription ::
  HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> TH.Code TH.Q (MintQuantity Text Currency)
quantityDescription vars (TH.AppT (TH.PromotedT name) t)
  | name == 'MintOrBurnSome = currency [||MintOrBurnSome||]
  | name == 'MintSome = currency [||MintSome||]
  | name == 'BurnSome = currency [||BurnSome||]
  where
    currency :: TH.Code TH.Q (Currency -> MintQuantity Text Currency) -> TH.Code TH.Q (MintQuantity Text Currency)
    currency name = [||$$name (Currency $$(textLiteral $ typeDescription vars t))||]
quantityDescription vars (TH.AppT (TH.AppT (TH.PromotedT name) qty) t)
  | name == 'Mint = currency [||Mint||]
  | name == 'Burn = currency [||Burn||]
  where
    currency ::
      TH.Code TH.Q (Text -> Currency -> MintQuantity Text Currency) -> TH.Code TH.Q (MintQuantity Text Currency)
    currency name = [||$$name $$(textLiteral $ typeDescription vars qty) (Currency $$(textLiteral $ typeDescription vars t))||]
quantityDescription vars (TH.SigT t _) = quantityDescription vars t
quantityDescription _ t = error ("quantityDescription " <> show t)

reifyList :: TypeReifier t -> TypeReifier t
reifyList reifyItem vars fieldName itemType = foldMap enumerateItem [1 .. listItems]
  where
    enumerateItem n = reifyItem vars [||$$(addSuffix fieldName n)||] itemType
    addSuffix prefix n = [||$$prefix <> $$(TH.unsafeCodeCoerce $ TH.litE . TH.StringL $ " " <> show n)||]

currencyDescriptionQuotes :: HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> TH.Code TH.Q [Currency]
currencyDescriptionQuotes vars =
  TH.unsafeCodeCoerce . pure . TH.ListE . map (TH.LitE . TH.StringL . Text.unpack) . typeDescriptions vars

typeDescriptions :: HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> [Text]
typeDescriptions vars (TH.AppT (TH.AppT TH.PromotedConsT t) ts) = typeDescription vars t : typeDescriptions vars ts
typeDescriptions vars TH.PromotedNilT = []
typeDescriptions vars (TH.SigT t _) = typeDescriptions vars t

maybeTypeDescriptionQuote :: HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> TH.Code TH.Q (Maybe Text)
maybeTypeDescriptionQuote vars (TH.AppT (TH.PromotedT j) t)
  | j == 'Just = [||Just $$(textLiteral $ typeDescription vars t)||]
maybeTypeDescriptionQuote vars (TH.PromotedT n) | n == 'Nothing = [||Nothing||]
maybeTypeDescriptionQuote vars (TH.SigT t _) = maybeTypeDescriptionQuote vars t

typeDescription :: HasCallStack => [(TH.Name, Maybe TH.Type)] -> TH.Type -> Text
typeDescription _ (TH.PromotedT name) = Text.pack (TH.nameBase name)
typeDescription _ (TH.LitT (TH.NumTyLit n)) = Text.pack (show n)
typeDescription _ (TH.LitT (TH.StrTyLit s)) = Text.pack s
typeDescription _ t@TH.PromotedTupleT {} = Text.pack (dropWhile (== '\'') $ TH.pprint t)
typeDescription vars (TH.AppT t v@(TH.VarT name)) =
  typeDescription vars t
  <> case lookupIndex name vars of
      Nothing -> ""
      _ -> " " <> typeDescription vars v
typeDescription vars (TH.AppT a b) = typeDescription vars a <> " " <> typeDescription vars b
typeDescription vars (TH.SigT t _) = typeDescription vars t
typeDescription _ (TH.ConT name) = Text.pack (TH.nameBase name)
typeDescription _ TH.ListT = "List of"
typeDescription vars (TH.VarT name) = case lookupIndex name vars of
      Nothing -> ""
      Just (Nothing, i) -> Text.pack (show $ succ i)
      Just (Just (TH.LitT (TH.StrTyLit s)), _) -> Text.pack s
      Just (Just t@TH.LitT {}, _) -> Text.pack (TH.pprint t)
      _ -> error ("Can't describe variable type " <> TH.pprint name)
typeDescription _ t = error (show t)

lookupIndex :: Eq k => k -> [(k, v)] -> Maybe (v, Int)
lookupIndex k = foldr f Nothing
  where
    f (k', v)
      | k == k' = const $ Just (v, 0)
      | otherwise = ((succ <$>) <$>)

typeNameAndArgs :: TH.Type -> (TH.Name, [TH.Type])
typeNameAndArgs (TH.ConT name) = (name, [])
typeNameAndArgs (TH.PromotedT name) = (name, [])
typeNameAndArgs (TH.AppT t t') = (<> [t']) <$> typeNameAndArgs t
typeNameAndArgs (TH.SigT t _) = typeNameAndArgs t
typeNameAndArgs t = error (TH.pprint t <> " is not a named type.")

stringLiteral :: String -> TH.Code TH.Q String
stringLiteral = TH.unsafeCodeCoerce . pure . TH.LitE . TH.StringL

textLiteral :: Text -> TH.Code TH.Q Text
textLiteral = TH.unsafeCodeCoerce . pure . TH.LitE . TH.StringL . Text.unpack

unKind :: TH.TyVarBndr flag -> TH.Name
unKind (TH.PlainTV name _) = name
unKind (TH.KindedTV name _ _) = name

sequenceExps :: forall x. TH.Q [TH.TExp x] -> TH.Q (TH.TExp [x])
sequenceExps = (>>= TH.examineCode . TH.unsafeCodeCoerce . pure . TH.ListE . map TH.unType)
