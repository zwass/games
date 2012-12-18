{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Go where

import Prelude as P
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

import Test.QuickCheck

import Solver
import PlayableGame

-- Go is a game between two players, called Black and White.

data Color = Black | White deriving (Show, Eq, Ord)

playerColor :: Player -> Color
playerColor PlayerOne = Black
playerColor PlayerTwo = White

-- Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.
data GoGame = GoGame {turn :: Player, position :: Position, history :: [Move]}
    deriving (Eq, Ord)

-- A point on the board where a horizontal line meets a vertical line is called an intersection.

newtype Intersection = I (Int, Int) deriving (Eq, Ord, Show)

-- Two intersections are said to be adjacent if they are connected by a horizontal or vertical line with no other position between them.

isAdjacent :: Intersection -> Intersection -> Bool
isAdjacent a b = S.member a $ adjacents b

adjacents :: Intersection -> Set Intersection
adjacents (I (x, y)) = S.fromList $ P.map I [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Go is played with playing tokens known as stones.
-- At any time in the game, each intersection on the board is in one and only one of the following three states:
--  1) empty;
--  2) occupied by a black stone;
--  3) occupied by a white stone. 
data IState = Empty | Stone Color deriving (Show, Eq, Ord)

isEmpty :: IState -> Bool
isEmpty = (== Empty)

stoneColor :: IState -> Color
stoneColor (Stone c) = c
stoneColor _ = error "invalid color"

-- A position consists of an indication of the state of each intersection.
type Position = Map Intersection IState

state :: Position -> Intersection -> Maybe IState
state p i = M.lookup i p

knownState :: Position -> Intersection -> IState
knownState p i = fromJust $ state p i

-- List of empty Intersectionss
empties :: GoGame -> [Intersection]
empties = M.keys . M.filter isEmpty . position

-- List of occupied Intersections
stones :: Position -> Position
stones = M.filter (not . isEmpty)

-- Map of Intersection occupied a stone owned by Player
pieces :: Player -> Position -> Position
pieces pl = M.filter ((== pColor) . stoneColor) . stones where pColor = playerColor pl

-- Check if an Intersection is on the board
onBoard :: Position -> Intersection -> Bool
onBoard p i = isJust $ state p i

-- Get valid adjacent Intersections
safeAdjs :: Position -> Intersection -> Set Intersection
safeAdjs p = S.filter (onBoard p) . adjacents

-- In a given position, two stones of the same color (or two empty intersections) are said to be connected if it is possible to pass from one to the other by a succession of stones of that color (or empty intersections, respectively) in which any two consecutive ones are adjacent.
connected :: Intersection -> Intersection -> Position -> Bool
connected a b p = S.member a $ connections b p

-- Check if two intersections are occupied by the same colored piece
friendly :: Position -> Intersection -> Intersection -> Bool
friendly p a b = f a == f b where f = knownState p

-- List all the Intersections that piece at the given Intersection is connected to
connections :: Intersection -> Position -> Set Intersection
connections i p = findFixedPoint (S.singleton i) (S.filter (friendly p i) . safeAdjs p)
    where
        findFixedPoint :: Ord a => Set a -> (a -> Set a) -> Set a
        findFixedPoint x gen
            | x' == x = x
            | otherwise = findFixedPoint x' gen
            where
                x' = S.union x $ S.unions $ S.toList $ S.map gen x

propConnected :: Intersection -> Intersection -> Position -> Property
propConnected a b p = connected a b p ==> connections a p == connections b p

-- In a given position, a liberty of a stone is an empty intersection adjacent to that stone or adjacent to a stone which is connected to that stone.
liberties :: Intersection -> Position -> Set Intersection
liberties i p = S.unions $ S.toList $ S.map emptyAdjs $ connections i p
    where
        emptyAdjs :: Intersection -> Set Intersection
        emptyAdjs = S.filter (isEmpty . knownState p) . safeAdjs p

propLiberties :: Intersection -> Intersection -> Position -> Property
propLiberties a b p = connected a b p ==> liberties a p == liberties b p


-- At the beginning of the game, the board is empty.
-- Black moves first. The players alternate thereafter.

goInitialPosition :: Int -> GoGame
goInitialPosition n = GoGame PlayerOne pos []
    where
        pos :: Position
        pos = M.fromList $ zip [I (x, y) | x <- [1..n], y <- [1..n]] (repeat Empty)

-- When it is their turn, a player may either pass or play.

goDoMove :: GoGame -> Move -> GoGame
goDoMove b 0 = GoGame (nextPlayer $ turn b) (position b) (0 : history b) -- pass
goDoMove b m = GoGame (nextPlayer $ turn b) (play m b)   (m : history b) -- play

-- Commit a move to the board
commitMove :: Move -> GoGame -> GoGame
commitMove m b = GoGame (nextPlayer $ turn b) (play m b) (m : history b)

goGenerateMoves :: GoGame -> [Move]
goGenerateMoves b = pass : plays b
    where
        pass = 0
        plays :: GoGame -> [Move]
        plays = P.map fst . P.filter (isEmpty . snd) . zip [1..] . M.elems . position

propGenMoves :: GoGame -> Bool
propGenMoves b = length (goGenerateMoves b) == length (empties b) + 1

-- Check if the piece at the given intersection has any liberties
hasLiberty :: Intersection -> Position -> Bool
hasLiberty i p = S.size (liberties i p) > 0

-- A play consists of the following steps (performed in the prescribed order):
--  (Playing a stone) Placing a stone of their color on an empty intersection (chosen subject to Rule 8).
--  (Capture) Removing from the board any stones of their opponent's color that have no liberties.
--  (Self-capture) Removing from the board any stones of their own color that have no liberties.

play :: Move -> GoGame -> Position
play m b = doCapture pl $ doCapture op $ playStone (moveToIntersection m b) pl p
  where
    p = position b
    pl = turn b
    op = nextPlayer pl

-- Play a stone owned by the given Player at the given Intersection
playStone :: Intersection -> Player -> Position -> Position
playStone i pl = M.insert i (Stone $ playerColor pl)

-- Remove all pieces owned by the given Player that do not have any liberties
doCapture :: Player -> Position -> Position
doCapture pl p = M.mapWithKey f p
    where
        pColor = playerColor pl
        f k (Stone c) 
            | c == pColor = if hasLiberty k p then Stone c else Empty
            | otherwise = Stone c
        f _ (Empty) = Empty

-- 7. Two consecutive passes end the game.
goPrimitive :: GoGame -> Value
goPrimitive b
    | [0, 0] `isPrefixOf` history b = evalPosition (position b)
    | otherwise = Undecided

-- 8. A player's territory consists of all the points the player has either occupied or surrounded.
-- 9. The player with more territory wins.
evalPosition :: Position -> Value
evalPosition p
    | score > 0 = Win
    | score < 0 = Lose
    | otherwise = Tie
    where
        score = scorePosition p

scorePosition :: Position -> Int
scorePosition p = M.size (pieces PlayerOne p) - M.size (pieces PlayerTwo p)

goWhoseTurn :: GoGame -> Player
goWhoseTurn = turn

-- Convert a Move into an Intersection
moveToIntersection :: Move -> GoGame -> Intersection
moveToIntersection m = snd . fromJust . find ((== m) . fst) . zip [1..] . M.keys . position

-- Convert a Intersection into an Intersection
intersectionToMove :: Intersection -> GoGame -> Move
intersectionToMove i = fst . fromJust . find ((== i) . snd) . zip [1..] . M.keys . position

instance SolvableGame GoGame where
  initialPosition = goInitialPosition 2
  doMove = goDoMove
  primitive = goPrimitive
  generateMoves = goGenerateMoves
  whoseTurn = goWhoseTurn

estimatePosition :: Position -> Value
estimatePosition p
    | score > 0 = Win
    | score < 0 = Lose
    | otherwise = Tie
    where
        score = scorePosition p

goEstimatePrimitive :: GoGame -> Value
goEstimatePrimitive = estimatePosition . position

{-
instance HardGame GoGame where
  estimatePrimitive = goEstimatePrimitive
-}

instance PlayableGame GoGame where
  showBoard = show
  showMoves = show


---



instance Show GoGame where
    show b = show $ position b


genIntersection :: GoGame -> Gen Intersection
genIntersection = elements . empties

genMove :: GoGame -> Gen Move
genMove b = do
    i <- genIntersection b
    return $ intersectionToMove i b

makeRandMove :: GoGame -> Gen GoGame
makeRandMove b = do
    m <- genMove b
    return $ goDoMove b m

instance Arbitrary GoGame where
    arbitrary = do
        b <- makeRandMove (goInitialPosition 5)
        c <- makeRandMove b
        d <- makeRandMove c
        makeRandMove d

instance Arbitrary Intersection where
    arbitrary = do
        b <- arbitrary :: Gen GoGame
        elements $ empties b

instance Arbitrary Position where
    arbitrary = do
        b <- arbitrary :: Gen GoGame
        return $ position b
