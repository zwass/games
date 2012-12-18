{-# OPTIONS_GHC -Wall #-}

module Player where

import Control.Monad.State.Lazy
import qualified Data.List as L
import qualified Data.Map as M

import Solver
import OneTwoTen
import TicTacToe
import Connect4

data ST a = ST { tree :: GameTree a, curPos :: a }

type PlayState a b = StateT (ST a) IO b

type PlayS a = State (GameTree a) a

initGame :: IO ()
initGame = do
  putStrLn "What would you like to play?"
  putStr $ unlines ["1) One, Two, Ten",
                    "2) Tic, Tac, Toe",
                    "3) Connect 4"]
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
    _ -> do { putStrLn "Unrecognized option"; initGame } where

parseMainMenuOption :: String -> Maybe Int
parseMainMenuOption s = case reads s of
  [(o,"")] -> Just o
  _ -> Nothing

playGame :: PlayableGame a => PlayState a ()
playGame = do
  s <- get
  let pos = curPos s
  case primitive pos of
    Undecided -> case whoseTurn pos of
      PlayerOne -> humanTurn
      PlayerTwo -> computerTurn
    Lose -> do
      lift $ putStrLn "Computer wins!"
      lift $ putStrLn ("Board:\n" ++ showBoard pos)
      return ()
    Tie -> do
      lift $ putStrLn "It's a tie!"
      lift $ putStrLn ("Board:\n" ++ showBoard pos)
      return ()
    Win -> do
      lift $ putStrLn "You win!"
      lift $ putStrLn ("Board:\n" ++ showBoard pos)
      return ()

humanTurn :: PlayableGame a => PlayState a ()
humanTurn = do
  s <- get
  let pos = curPos s
  let moves = generateMoves pos
  lift $ putStr ("Board:\n" ++ showBoard pos)
  lift $ putStrLn ("Possible moves: " ++ show moves)
  move <- lift $ getMove moves
  put $ ST (tree s) (doMove pos move)
  playGame

computerTurn :: PlayableGame a => PlayState a ()
computerTurn = do
  s <- get
  let pos = curPos s
  let moves = generateMoves pos
  best <- bestMove moves pos
  put $ ST (tree s) (doMove pos best)
  lift $ putStrLn ("Board:\n" ++ showBoard pos)
  lift $ putStrLn ("Computer chose move " ++ show best)
  playGame
  
bestMove :: SolvableGame a => [Move] -> a -> PlayState a Move
bestMove ms pos = do
  s <- get
  let nextPos = foldr (\m xs -> (m, doMove pos m):xs) [] ms
      nextVals = map (\(m, p) ->
                       (m, M.findWithDefault
                           (error "Tree not fully explored")
                           p (tree s))) nextPos
      (best, _) = L.minimumBy (\(_, v1) (_, v2) -> compare v1 v2) nextVals
  return best
                                       
parseMove :: String -> Maybe Move
parseMove s = case reads s of
  [(m,"")] -> Just m
  _ -> Nothing

getMove :: [Move] -> IO Move
getMove valid = do
  putStr "Which move would you like? "
  moveS <- getLine
  let move = parseMove moveS
  case move of
    Just m | m `elem` valid -> return m
    _ -> do { putStrLn "That's not a valid move..."; getMove valid }

process :: PlayableGame a => String -> PlayState a ()
process x | x == "q" = return ()
process x = do { lift $ putStrLn ("You said " ++ x); playGame }

main :: IO ()
main = do { initGame; main }
