{-# OPTIONS -Wall #-}

module OneTwoTen where

import Data.Binary

import Solver

data OTTBoard = OTTBoard {turn :: Player,
                          ottBoard :: Integer}
                deriving (Show, Read, Eq, Ord)

instance Binary OTTBoard where
  put (OTTBoard a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (OTTBoard a b)

boardSize :: Integer
boardSize = 10

instance SolvableGame OTTBoard where
  initialPosition = OTTBoard PlayerOne 0
  doMove b m =
    OTTBoard (nextPlayer . turn $ b) (m + (ottBoard b))
  primitive = ottPrimitive
  generateMoves _ = [1, 2]
  whoseTurn = turn

instance PlayableGame OTTBoard where
  showBoard b = "Current: " ++ show (ottBoard b)
                ++ " Goal: " ++ show boardSize ++ "\n"
  showMoves = show

ottPrimitive :: OTTBoard -> Value
ottPrimitive b
  | ottBoard b < boardSize = Undecided
  | turn b == PlayerOne = Lose
  | otherwise = Win
