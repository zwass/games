{-# OPTIONS_GHC -Wall #-}

module Solver (SolvableGame(..), PlayableGame(..),
               Player (..), Move, Value(..), GameTree,
               solveGame, getValue, nextPlayer, loadOrSolveTree) where

import qualified Control.Exception as E
--import System.IO.Error (catchIOError)
import Data.Binary
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State as S

--For storing the value of a move/state
--Win denotes a win for player one
--Lose denotes a win for player two
data Value = Undecided | Lose | Tie | Win
           deriving (Show, Read, Eq, Ord)

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

--For storing whose turn it is
data Player = PlayerOne | PlayerTwo
            deriving (Show, Read, Eq, Ord)

instance Binary Player where
  put PlayerOne = putWord8 0
  put PlayerTwo = putWord8 1
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return PlayerOne
      1 -> return PlayerTwo
      _ -> fail "no parse"

nextPlayer :: Player -> Player
nextPlayer PlayerOne = PlayerTwo
nextPlayer PlayerTwo = PlayerOne

type Move = Integer

class (Ord a, Show a) => SolvableGame a where -- Parametrized by game state
  initialPosition :: a
  doMove :: a -> Move -> a
  primitive :: a -> Value
  generateMoves :: a -> [Move]
  whoseTurn :: a -> Player

class SolvableGame a => PlayableGame a where
  showBoard :: a -> String
  showMoves :: a -> String

type GameTree a = Map a Value

type SolverState a = State (GameTree a) Value

playerOneMax :: [Value] -> Value
playerOneMax vals | null vals = error "No children from non-Leaf node"
                  | otherwise = maximum vals

playerTwoMax :: [Value] -> Value
playerTwoMax vals | null vals = error "No children from non-Leaf node"
                  | otherwise = minimum . filter (/= Undecided) $ vals

solveGame :: SolvableGame a => GameTree a
solveGame = S.execState (solve initialPosition) M.empty where
  solve :: SolvableGame a => a -> SolverState a
  solve pos = do
    s <- S.get
    -- Try to use memoized value
    case M.lookup pos s of
      -- Just use memoized value
      Just val -> return val
      -- Compute value
      Nothing -> case primitive pos of
        -- At an internal node. Recursively solve for value.
        Undecided -> do
          let children = map (doMove pos) (generateMoves pos)
              turn = whoseTurn pos
              maxFun = if turn == PlayerOne then playerOneMax else playerTwoMax
          childVals <- sequence $ map solve children
          let val = maxFun childVals
          -- Memoize the computed value
          S.modify (M.insert pos val) -- Memoize
          return val
        -- At a leaf
        val -> do
          S.modify (M.insert pos val) -- Memoize the leaf value
          return val -- Leaf

getValue :: SolvableGame a => a -> Value
getValue x = M.findWithDefault (error $ "No value for " ++ show x) x solveGame
            
loadOrSolveTree :: (Binary a, SolvableGame a) =>
                   FilePath -> IO (GameTree a)
loadOrSolveTree fname = do
  putStrLn $ "Loading game tree from file " ++ fname
  t <- E.catch (decodeFile fname) handler
  putStrLn $ "Game tree loaded"
  return t where
    handler :: (Binary a, SolvableGame a) =>
               E.SomeException -> IO (GameTree a)
    handler _ = do
      putStrLn "Error loading game tree... Generating new tree."
      let t = solveGame
      encodeFile fname t
      return t
