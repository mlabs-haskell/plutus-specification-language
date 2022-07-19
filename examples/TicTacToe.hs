{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TicTacToe where

import Data.Map (Map)
import Plutarch.PSL

-- TODO: The type system should know that Red/Blue are players
-- so that we can prove that `gameWon` is called by a player
-- and not `Blank`.
data Slot = Red | Blue | Blank
  deriving stock (Eq)

-- Grid of TTT slots
newtype Grid a = (Num a) => Map a (Map a Slot)

-- TODO: Use type-nats so that you can prove that the given
-- grid index is correct
initState :: Grid Int
initState =
  fromList
    [ (0, fromList [(0, Blank), (1, Blank), (2, Blank)])
    , (1, fromList [(0, Blank), (1, Blank), (2, Blank)])
    , (2, fromList [(0, Blank), (1, Blank), (2, Blank)])
    ]

data GameDatum ef = GameDatum
  { state :: ef /$ Grid Int
  , redPkh :: ef /$ EPubKeyHash
  , bluePkh :: ef /$ EPubKeyHash
  , value :: ef /$ EValue
  }
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data Turn = (,) Int Int
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data GameCase ef where
  GameInit :: ef /$ EPubKeyHash -> ef /$ EPubkeyHash -> ef /$ EValue -> GameCase ef
  RedPlay :: ef /$ GameDatum -> ef /$ Turn -> GameCase ef
  BluePlay :: ef /$ GameDatum -> ef /$ Turn -> GameCase ef
  RedWon :: ef /$ GameDatum -> GameCase ef
  BlueWon :: ef /$ GameDatum -> GameCase ef
  deriving stock (Generic)
  deriving anyclass (EIsNewtype)

data GameProtocol

instance Protocol GameProtocol GameDatum where
  specification =
    Specification @GameDatum @GameCase
      ( `ematch`
          \case
            GameInit dat@GameDatum{..} -> MonoidDo.do
              requireOwnOutput $ econ $ EOwnUTXO value $ econ dat
              requireSignature redPkh
              requireSignature bluePkh
            RedPlay dat turn -> redPlay dat val turn
            BluePlay dat turn -> bluePlay dat turn
            RedWon dat -> redWon dat
            BlueWon dat -> blueWon dat
      )
   where
    redPlay = gamePlay Red
    bluePlay = gamePlay Blue
    gamePlay slot dat@GameDatum{..} (i, j) = MonoidDo.do
      requireOwnInput $ econ $ EOwnUTXO value $ econ $ dat{state = insert i (insert j Blank $ state ! i) state}
      requireOwnOutput $ econ $ EOwnUTXO value $ econ $ dat{state = insert i (insert j slot $ state ! i) state}
    redWon = gameWon Red
    blueWon = gameWon Blue
    gameWon slot dat@GameDatum{..} =
      if checkGameWon state slot
        then ematch slot \case
          Blank -> Undefined
          _ -> requireWon dat value
        else Undefined
    requireWon GameDatum{..} = createOutput $ toAddress (fromPkh redPkh) value mempty
    checkGameWon state slot = foldl (||) (checkRowWon state slot) wins
    checkRowWon state slot = foldl (&&) $ (slot ==) . (\(i, j) -> state ! i ! j)
    wins =
      [ [(0, 0), (0, 1), (0, 2)]
      , [(1, 0), (1, 1), (1, 2)]
      , [(2, 0), (2, 1), (2, 2)]
      , [(0, 0), (1, 0), (2, 0)]
      , [(0, 1), (1, 1), (2, 1)]
      , [(0, 2), (1, 2), (2, 2)]
      , [(0, 0), (1, 1), (2, 2)]
      , [(0, 2), (1, 1), (2, 0)]
      ]
