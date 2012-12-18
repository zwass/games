{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module Games.Connect4 where

import Data.Binary
import Data.List (transpose, group, intercalate)
import Test.HUnit

import Solver
import PlayableGame

instance SolvableGame C4Board where
  initialPosition = c4GetInitialPosition
  doMove = c4DoMove
  primitive = c4Primitive
  generateMoves = c4GenerateMoves
  whoseTurn = c4WhoseTurn

instance PlayableGame C4Board where
  showBoard b = unlines $ colMarkers : board ++ [divider] where
    colMarkers = " " ++ intercalate " " (map show [1..boardWidth])
    board = map (("|" ++) . (++ "|")) rows
    maxLen = 1
    divider = take (maxLen * boardWidth + (boardWidth + 1)) (repeat '=') ++ "\n"
    rows = map (intercalate "|") (map (map show) $ transpose b)
                       
  showMoves = show

data Piece = R | B | E
           deriving (Eq, Ord)

instance Binary Piece where
  put R = putWord8 0
  put B = putWord8 1
  put E = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return R
      1 -> return B
      2 -> return E
      _ -> fail "no parse"

instance Show Piece where
  show R = "R"
  show B = "B"
  show E = " "
  
type C4Board = [[Piece]]

boardWidth :: Int
boardWidth = 4

boardHeight :: Int
boardHeight = 4

c4GetInitialPosition :: C4Board
c4GetInitialPosition = take boardWidth . repeat $
                       take boardHeight (repeat E)

c4GenerateMoves :: C4Board -> [Integer]
c4GenerateMoves = foldr checkE [] . zip [1..] where
  checkE (i, col) ms = if head col == E then i : ms else ms

c4WhoseTurn :: C4Board -> Player
c4WhoseTurn b = if fst count == snd count then PlayerOne else PlayerTwo where
  count = foldr countFun (0 :: Int, 0 :: Int) $ concat b
  countFun R (cr, cb) = (cr + 1, cb)
  countFun B (cr, cb) = (cr, cb + 1)
  countFun _ cs = cs

c4DoMove :: C4Board -> Move -> C4Board
c4DoMove b m = take (m' - 1) b ++ [newCol] ++ drop m' b  where
  m' = fromInteger m
  piece = if c4WhoseTurn b == PlayerOne then R else B
  oldCol = b !! (m' - 1)
  newCol = reverse . take boardHeight . (++ repeat E) . reverse $ 
           (piece : dropWhile (== E) oldCol)

reflect :: [[a]] -> [[a]]
reflect = map reverse

pieceAt :: C4Board -> (Int, Int) -> Piece
pieceAt b (row, col) = b !! (row-1) !! (col-1)

checkTie :: C4Board -> Bool
checkTie = not . (E `elem`) . concat

--Generate rows/diags and pass them to this function to check for 3
--in a row-ness
checkValue :: [Piece] -> Value
checkValue p = case (filter (\l -> length l >= 4) (group p)) of
  [R:_] -> Win
  [B:_] -> Lose
  _ -> Undecided

getRows :: C4Board -> [[Piece]]
getRows = id

getDiag :: C4Board -> (Int, Int) -> [Piece]
getDiag b (row, col)
  | row > boardHeight || col > boardWidth = []
  | otherwise = pieceAt b (row, col) : getDiag b (row+1, col+1)

getDiags :: Int -> C4Board -> [[Piece]]
getDiags mx b = map (getDiag b) $ zip [1..mx] (repeat 1)

--Get all the possible rows/columns and diagonals a player could have
--won on. We can use transpose and reflected to get the columns from
--the rows of the transpose, other diagonal from reflected.
getAllPossibilities :: C4Board -> [[Piece]]
getAllPossibilities b = getDiags boardWidth b ++ getRows b ++
                        getDiags boardHeight bt ++
                        getRows bt ++ getDiags boardWidth br
                        where bt = transpose b
                              br = reflect b

c4Primitive :: C4Board -> Value
c4Primitive b = if val == Undecided && checkTie b then Tie else val where
  val = maximum . map checkValue $ getAllPossibilities b

board1 :: C4Board
board1 = [[R, R, R, R],
          [E, E, E, E],
          [E, E, E, E],
          [E, E, E, E]]

board2 :: C4Board
board2 = [[B, B, B, B],
          [E, E, E, E],
          [E, E, E, E],
          [E, E, E, E]]

board3 :: C4Board
board3 = [[R, E, E, E],
          [E, R, E, E],
          [E, E, R, E],
          [E, E, E, R]]

board4 :: C4Board
board4 = [[R, B, B, R],
          [R, B, B, R],
          [B, R, R, B],
          [B, R, R, B]]

testPrimitive :: Test
testPrimitive = TestList $
                [c4Primitive board1 ~=? Win,
                 c4Primitive (transpose board1) ~=? Win,
                 c4Primitive (reflect board1) ~=? Win,
                 c4Primitive board2 ~=? Lose,
                 c4Primitive (transpose board2) ~=? Lose,
                 c4Primitive (reflect board2) ~=? Lose,
                 c4Primitive board3 ~=? Win,
                 c4Primitive (transpose board3) ~=? Win,
                 c4Primitive (reflect board3) ~=? Win,
                 c4Primitive board4 ~=? Tie]

