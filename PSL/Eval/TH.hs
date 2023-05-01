{-# LANGUAGE TemplateHaskellQuotes #-}

module PSL.Eval.TH (unsound, mkConvHelper, mkConvHelper') where

import Language.Haskell.TH

unsound :: String -> err
unsound ty = error $ "Type '" <> ty <> "' is unsound"

mkConvHelper :: String -> Q Type -> Q [Dec]
mkConvHelper nm ty = do
  let conName = mkName ('V' : nm)
  let valTypeName = mkName "Val"
  let partialTypeName = mkName "Partial"
  let vNeutralConName = mkName "VNeutral"
  let pNormalConName = mkName "PNormal"
  let pNeutralConName = mkName "PNeutral"
  let unsoundName = 'unsound
  let funName = mkName ("into" ++ nm)
  xVar <- newName "x"
  ty' <- ty
  pure
    [ SigD funName (ArrowT `AppT` ConT valTypeName `AppT` (ConT partialTypeName `AppT` ty'))
    , FunD
        funName
        [ Clause [ConP conName [] [VarP xVar]] (NormalB $ AppE (ConE pNormalConName) (VarE xVar)) []
        , Clause [ConP vNeutralConName [] [VarP xVar]] (NormalB $ AppE (ConE pNeutralConName) (VarE xVar)) []
        , Clause [WildP] (NormalB $ AppE (VarE unsoundName) (LitE $ StringL nm)) []
        ]
    ]

mkConvHelper' :: String -> Q [Dec]
mkConvHelper' nm = mkConvHelper nm do
  let tyName = mkName nm
  pure $ ConT tyName
