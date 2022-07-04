{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module TicTacToe where

import PSL

data Slot = Red | Blue | Blank

data Grid
  = (,,)
      (Slot, Slot, Slot)
      (Slot, Slot, Slot)
      (Slot, Slot, Slot)

data GameDatum f = GameDatum
  { state :: Ef f Grid
  , redPkh :: Ef f EPubKeyHash
  , bluePkh :: Ef f EPubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data Turn = (,) Int Int
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data GameCase f where
  GameInit :: GameCase f
  RedPlay :: Ef f GameDatum -> Ef f Turn -> GameCase f
  BluePlay :: Ef f GameDatum -> Ef f Turn -> GameCase f
  RedWon :: GameCase f
  BlueWon :: GameCase f
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

gameCases ::
  EPSL edsl =>
  Term edsl EPubKeyHash ->
  Term edsl EPubKeyHash ->
  Term edsl EInteger ->
  Term edsl GameCase ->
  Term edsl (EDiagram GameDatum)
gameCases redPkh bluePkh adaCount =
  ( `ematch`
      \case
        GameInit -> MonoidDo.do
          let initDatum = let initRow = (Blank, Blank, Blank) in GameDatum{state = (initRow, initRow, initRow), ..}
          let ownUtxo = econ $ EOwnUTXO (mkAda adaCount) $ econ initDatum
          requireOwnInput ownUtxo
          createOwnOutput ownUtxo
        -- TODO: Work out the logic for when each player won each game
        _ -> undefined
  )

data GameProtocol

instance Protocol GameProtocol GameDatum where
  specification = Specification @GameDatum @GameCase gameCases
