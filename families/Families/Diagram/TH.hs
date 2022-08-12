-- | This module exports the templates for automatic instance deriving of a 'TransactionTypeDiagram' value from a
-- transaction type. The most common way to use it would be
--
-- > import qualified Families.Diagram.TH
-- > instance Transaction MyTransaction where ...
-- > myTransactionDiagram = $(Families.Diagram.TH.transactionDiagram ''MyTransaction)

{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}

module Families.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType) where

import Families (Transaction (Inputs, Mints, Outputs))
import Families.Diagram qualified as D
import Families.Diagram (
  TransactionTypeDiagram (..),
  InputFromScript (..),
  OutputToScript (..),
  Wallet (..),
  Currency (..),
  )

import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (liftTyped)

type TypeReifier a = TH.Name -> TH.Name -> [TH.Name] -> TH.Type -> Maybe (TH.Code TH.Q a)

untypedDiagramForTransactionType :: TH.Type -> TH.Q TH.Exp
untypedDiagramForTransactionType = TH.unTypeCode . diagramForTransactionType

diagramForTransactionType :: TH.Type -> TH.Code TH.Q TransactionTypeDiagram
diagramForTransactionType t = TH.Code $ do
  TH.ClassI _ ins <- TH.reify ''Transaction
  let pred = \case
        TH.InstanceD _ _ t' _ | TH.AppT (TH.ConT ''Transaction) t == t' -> True
        _ -> False
  TH.reifyInstances ''Transaction [t] >>= \case
    [TH.InstanceD{}] -> pure ()
    [] -> TH.reportError ("Missing instance Transaction '" <> TH.pprint t)
    _ -> TH.reportError ("Multiple instances Transaction '" <> TH.pprint t)
  TH.examineCode
    [||
      TransactionTypeDiagram
        $$(textLiteral $ typeDescription [] t)
        (Map.fromList $$(reifyMapField t ''Inputs reifyScriptInput t))
        (Map.fromList $$(reifyMapField t ''Outputs reifyScriptOutput t))
        (Map.fromList $$(reifyMapField t ''Inputs reifyWallet t))
        (Map.fromList $$(reifyMapField t ''Outputs reifyWallet t))
    ||]
{-
    [||
      TransactionTypeDiagram {
        transactionName = Text.pack $$(TH.unsafeCodeCoerce $ pure $ TH.LitE $ TH.StringL $ TH.pprint t),
        scriptInputs = Map.fromList [],
        scriptOutputs = Map.fromList [],
        walletInputs = Map.fromList [],
        walletOutputs = Map.fromList []
        }
    ||]
-}

reifyMapField :: forall a. TH.Type -> TH.Name -> TypeReifier a -> TH.Type -> TH.Code TH.Q [(Text, a)]
reifyMapField t assocTypeName reifyType recordType = TH.Code $ do
  let description = TH.pprint assocTypeName <> " '" <> TH.pprint t
  typeName <- TH.reifyInstances assocTypeName [t] >>= \case
    [TH.TySynInstD (TH.TySynEqn _ _ t')] -> pure (typeName t')
    [d] -> ''Transaction <$ TH.reportError ("Weird instance " <> description <> ": " <> show d)
    [] -> ''Transaction <$ TH.reportError ("Missing instance " <> description)
    _ -> ''Transaction <$ TH.reportError ("Multiple instances " <> description)
  (TH.TyConI tyCon) <- TH.reify typeName
  (tyConName, tyVars, cons) <- case tyCon of
    TH.DataD _ nm tyVars _kind cons _ -> pure (nm, tyVars, cons)
    d -> (typeName, [], []) <$ TH.reportError ("Unexpected declaration of " <> TH.pprint tyCon <> ": " <> show d)
  (walletVar, scriptVar, rest) <- case reverse tyVars of
    a:b:vs -> pure (unKind a, unKind b, unKind <$> reverse vs)
    _ ->
      (tyConName, tyConName, []) <$ TH.reportError ("Less than two type vars on " <> TH.pprint tyCon <> " declaration")
  fields <- case cons of
    [TH.RecC _ flds] -> pure flds
    [_] -> [] <$ TH.reportError ("Non-record data " <> TH.pprint tyConName <> " declaration")
    [] -> [] <$ TH.reportError ("Empty data " <> TH.pprint tyConName <> " declaration")
    _ -> [] <$ TH.reportError ("Multiple data " <> TH.pprint tyConName <> " constructors")
  let reifyField :: TH.VarBangType -> Maybe (TH.Q (TH.TExp (Text, a)))
      reifyField (name, _, fieldType) =
        TH.examineCode . (flip TH.bindCode $ addName name) . TH.examineCode <$> reifyType scriptVar walletVar rest fieldType
      addName :: TH.Name -> TH.TExp a -> TH.Code TH.Q (Text, a) 
      addName name a = [|| (Text.pack $$(stringLiteral $ TH.nameBase name), $$(TH.Code $ pure a)) ||]
  sequenceExps $ sequenceA $ mapMaybe reifyField fields

reifyScriptInput :: HasCallStack => TypeReifier InputFromScript
reifyScriptInput scriptVar _walletVar vars
  (TH.AppT
    (TH.AppT
      (TH.AppT (TH.VarT s) scriptType)
      redeemerType)
    currencies)
  | s == scriptVar = Just
    [||
      InputFromScript $$(textLiteral $ typeDescription vars scriptType) $$(textLiteral $ typeDescription vars redeemerType) mempty $$(currencyDescriptions vars currencies)
    ||]
reifyScriptInput _ _ _ _ = Nothing

reifyWallet :: HasCallStack => TypeReifier Wallet
reifyWallet _scriptVar walletVar vars (TH.AppT (TH.VarT w) currencies)
  | w == walletVar = Just
    [||
      Wallet (Text.pack "W") $$(currencyDescriptions vars currencies)
    ||]
reifyWallet _ _ _ _ = Nothing

reifyScriptOutput :: HasCallStack => TypeReifier OutputToScript
reifyScriptOutput scriptVar _walletVar vars
  (TH.AppT
    (TH.AppT (TH.VarT s) scriptType)
    currencies)
  | s == scriptVar = Just
    [||
      OutputToScript $$(textLiteral $ typeDescription vars scriptType) mempty $$(currencyDescriptions vars currencies)
    ||]
reifyScriptOutput _ _ _ _ = Nothing

currencyDescriptions :: HasCallStack => [TH.Name] -> TH.Type -> TH.Code TH.Q [Currency]
currencyDescriptions vars =
  TH.unsafeCodeCoerce . pure . TH.ListE . map (TH.LitE . TH.StringL . Text.unpack) . typeDescriptions vars

typeDescriptions :: HasCallStack => [TH.Name] -> TH.Type -> [Text]
typeDescriptions vars (TH.AppT (TH.AppT TH.PromotedConsT t) ts) = typeDescription vars t : typeDescriptions vars ts
typeDescriptions vars TH.PromotedNilT = []
typeDescriptions vars (TH.SigT t _) = typeDescriptions vars t

typeDescription :: HasCallStack => [TH.Name] -> TH.Type -> Text
typeDescription _ (TH.PromotedT name) = Text.pack (TH.nameBase name)
typeDescription _ (TH.LitT (TH.NumTyLit n)) = Text.pack (show n)
typeDescription _ t@TH.PromotedTupleT{} = Text.pack (dropWhile (== '\'') $ TH.pprint t)
typeDescription vars (TH.AppT t (TH.VarT v)) =
  typeDescription vars t <> foldMap ((" " <>) . Text.pack . show . succ) (elemIndex v vars)
typeDescription vars (TH.AppT a b) = typeDescription vars a <> " " <> typeDescription vars b
typeDescription vars (TH.SigT t _) = typeDescription vars t
typeDescription _ t = error ("Can't describe type " <> TH.pprint t)

typeName :: TH.Type -> TH.Name
typeName (TH.ConT name) = name
typeName (TH.AppT t TH.VarT{}) = typeName t
typeName (TH.SigT t _) = typeName t
typeName t = error (TH.pprint t <> " is not a named type.")

stringLiteral :: String -> TH.Code TH.Q String
stringLiteral = TH.unsafeCodeCoerce . pure . TH.LitE . TH.StringL

textLiteral :: Text -> TH.Code TH.Q Text
textLiteral = TH.unsafeCodeCoerce . pure . TH.LitE . TH.StringL . Text.unpack

unKind :: TH.TyVarBndr flag -> TH.Name
unKind (TH.PlainTV name _) = name 
unKind (TH.KindedTV name _ _) = name 

sequenceExps :: forall x. TH.Q [TH.TExp x] -> TH.Q (TH.TExp [x])
sequenceExps = (>>= TH.examineCode . TH.unsafeCodeCoerce . pure . TH.ListE . map TH.unType)
