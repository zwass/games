{-
    To avoid recomputing solved games, we can write game trees to disk and
    then simply reread them in the future.
-}
module SerializableGame (loadOrSolveTree) where

import qualified Control.Exception as E
import Data.Binary

import Solver


loadOrSolveTree :: (Binary a, SolvableGame a) => FilePath -> IO (GameTree a)
loadOrSolveTree fname = do
  putStrLn $ "Loading game tree from file " ++ fname
  t <- E.catch (decodeFile fname) handler
  putStrLn "Game tree loaded"
  return t where
    handler :: (Binary a, SolvableGame a) =>
               E.SomeException -> IO (GameTree a)
    handler _ = do
      putStrLn "Error loading game tree... Generating new tree."
      let t = solveGame
      encodeFile fname t
      return t


instance Binary Value where
  put Undecided = putWord8 0
  put Lose = putWord8 1
  put Tie = putWord8 2
  put Win = putWord8 3
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Undecided
      1 -> return Lose
      2 -> return Tie
      3 -> return Win
      _ -> fail "no parse"


instance Binary Player where
  put PlayerOne = putWord8 0
  put PlayerTwo = putWord8 1
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return PlayerOne
      1 -> return PlayerTwo
      _ -> fail "no parse"
