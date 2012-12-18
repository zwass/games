{-# LANGUAGE IncoherentInstances #-}

module PlayableGame (PlayableGame(..), ST(..), playGame) where

import Prelude as P
import Control.Monad.State as S
import Data.List as L
import Data.Map as M

import Solver


class SolvableGame a => PlayableGame a where
  showBoard :: a -> String
  showMoves :: a -> String
  chooseBest :: [Move] -> a -> GameTree a -> Move
  chooseBest ms pos t = best
    where
      nextPos = P.foldr (\m xs -> (m, doMove pos m):xs) [] ms
      nextVals = P.map (\(m, p) -> (m, M.findWithDefault (error "Tree not fully explored") p t)) nextPos
      (best, _) = L.minimumBy (\(_, v1) (_, v2) -> compare v1 v2) nextVals
  {-
  chooseBest ms pos t = case find ((== Win) . getValue . snd) (nextPos pos ms) of
                        Nothing -> head ms
                        Just (m, _) -> m
  -}


{-
nextPos :: SolvableGame a => a -> [Move] -> [(Move, a)]
nextPos p = foldr (\m xs -> (m, doMove p m):xs) []
-}



{-
foo t ms pos = best
where
  nextVals :: [(Move, Value)]
  nextVals = map (\(m, p) ->
                   (m, M.findWithDefault
                       (error "Tree not fully explored")
                       p t)) (nextPos pos ms)
  (best, _) = L.minimumBy (\(_, v1) (_, v2) -> compare v1 v2) nextVals
-}
data ST a = ST { tree :: GameTree a, curPos :: a }

type PlayState a b = StateT (ST a) IO b

type PlayS a = State (GameTree a) a

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
  
-- TODO: This function is a little weird.
-- We may want to pass the tree to chooseBest somehow...
bestMove :: PlayableGame a => [Move] -> a -> PlayState a Move
bestMove ms pos = do
    s <- S.get
    return $ chooseBest ms pos (tree s)

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
