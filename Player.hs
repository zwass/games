{-# OPTIONS_GHC -Wall #-}

module Player where

import Control.Monad.State.Lazy
import Data.Map as M

import Solver
import PlayableGame
import SerializableGame

import Games.OneTwoTen
import Games.TicTacToe
import Games.Connect4
import Games.Go


initGame :: IO ()
initGame = do
  putStrLn "What would you like to play?"
  putStr $ unlines ["1) One, Two, Ten",
                    "2) Tic, Tac, Toe",
                    "3) Connect 4",
                    "4) Go"]
  optS <- getLine
  let opt = parseMainMenuOption optS
  case opt of
    Just 1 -> do
      t <- loadOrSolveTree "ott_tree" :: IO (GameTree OTTBoard)
      evalStateT playGame $ ST t initialPosition
    Just 2 -> do
      t <- loadOrSolveTree "ttt_tree" :: IO (GameTree TTTBoard)
      evalStateT playGame $ ST t initialPosition
    Just 3 -> do
      t <- loadOrSolveTree "c4_tree" :: IO (GameTree C4Board)
      evalStateT playGame $ ST t initialPosition
    Just 4 -> evalStateT playGame $ ST M.empty (initialPosition :: GoGame)
    _ -> do { putStrLn "Unrecognized option"; initGame }

parseMainMenuOption :: String -> Maybe Int
parseMainMenuOption s = case reads s of
  [(o,"")] -> Just o
  _ -> Nothing

                                       
runPlayer :: IO ()
runPlayer = do { initGame; runPlayer}
