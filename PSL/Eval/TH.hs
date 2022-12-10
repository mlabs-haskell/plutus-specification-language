{-# LANGUAGE TemplateHaskellQuotes #-}

module PSL.Eval.TH (mkConvHelper, mkConvHelper') where

import Language.Haskell.TH

mkConvHelper :: String -> Q Type -> Q [Dec]
mkConvHelper nm ty = do
  val <-
    lookupValueName ('V' : nm) >>= \case
      Nothing -> error $ "Constructor 'V" <> nm <> "' not found"
      Just x -> pure x
  valTypeName <-
    lookupTypeName "Val" >>= \case
      Nothing -> error "Type 'Val' not found"
      Just x -> pure x
  let funName = mkName ("into" ++ nm)
  xVar <- newName "x"
  ty' <- ty
  let errorFn = 'error
  pure
    [ SigD funName (ArrowT `AppT` ConT valTypeName `AppT` ty')
    , FunD
        funName
        [ Clause [ConP val [] [VarP xVar]] (NormalB $ VarE xVar) []
        , Clause [WildP] (NormalB $ AppE (VarE errorFn) (LitE $ StringL $ "absurd: " <> nm <> " is not " <> nm)) []
        ]
    ]

mkConvHelper' :: String -> Q [Dec]
mkConvHelper' nm = mkConvHelper nm do
  tyName <-
    lookupTypeName nm >>= \case
      Nothing -> error $ "Type '" <> nm <> "' not found"
      Just x -> pure x
  pure $ ConT tyName
