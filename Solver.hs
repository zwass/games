{-# OPTIONS_GHC -Wall #-}

module Solver where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State as S

--For storing the value of a move/state
--Win denotes a win for player one
--Lose denotes a win for player two
data Value = Undecided | Lose | Tie | Win
           deriving (Show, Read, Eq, Ord)

--For storing whose turn it is
data Player = PlayerOne | PlayerTwo
            deriving (Show, Read, Eq, Ord)

nextPlayer :: Player -> Player
nextPlayer PlayerOne = PlayerTwo
nextPlayer PlayerTwo = PlayerOne

data GameState a = GameState {
  gs :: a -- Stores state of the game. Should allow implementing game to decide
          -- whose turn it is.
  }

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
            
