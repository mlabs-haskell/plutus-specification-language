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

data GameDatum ef = GameDatum
  { state :: ef /$ Grid
  , redPkh :: ef /$ EPubKeyHash
  , bluePkh :: ef /$ EPubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data Turn = (,) Int Int
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data GameCase ef where
  GameInit ::
    Term edsl EPubKeyHash ->
    Term edsl EPubKeyHash ->
    Term edsl EInteger ->
    GameCase ef
  RedPlay :: ef /$ GameDatum -> ef /$ Turn -> GameCase ef
  BluePlay :: ef /$ GameDatum -> ef /$ Turn -> GameCase ef
  RedWon :: GameCase ef
  BlueWon :: GameCase ef
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

gameCases ::
  EPSL edsl =>
  Term edsl GameCase ->
  Term edsl (EDiagram GameDatum)
gameCases =
  ( `ematch`
      \case
        GameInit redPkh bluePkh adaCount -> MonoidDo.do
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
