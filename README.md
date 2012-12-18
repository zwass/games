# games

games is a library for solving and playing games.

## Setup

Compile the program via:

```
cabal install split
ghc --make Main.hs
```

# Overview

## Solving Games

To solve a game, one needs to define a game as instance of a SolvableGame.

```haskell
-- Solver.hs

class (Ord a, Show a) => SolvableGame a where -- Parametrized by game state
  -- Generate the initial position of the game
  initialPosition :: a
  -- Perform a Move in the game to get a new game
  doMove :: a -> Move -> a
  -- Evaluate whether a game is Won, Lost, Tied, or Undecided
  primitive :: a -> Value
  -- Given a game, generate a list of legal moves
  generateMoves :: a -> [Move]
  -- Given a game, report whose turn it is
  whoseTurn :: a -> Player
```

Once defined, a game can be solved by calling `solveGame`.

## Playing Games

To make a game playable, one needs to define a game as an instance of PlayableGame

```haskell
-- PlayableGame.hs

class SolvableGame a => PlayableGame a where
  -- Print out a human readable representation of the Game
  showBoard :: a -> String
  -- Show the list of moves the player can make
  showMoves :: a -> String
```

Once defined, one can add the game to `Player.hs` to play against the computer.

### A note on hard games

Some games, like Go, are solvable but, prohibitively expensive to solve. For these games, chooseBest can be overridden to make a reasonable, but possibly incorrect choice, instead of relying on a solved game tree to make decisions.


```haskell
class SolvableGame a => PlayableGame a where
  -- By default, this uses a solved game tree to always make the best move
  -- Should be overridden for hard games
  chooseBest :: [Move] -> a -> GameTree a -> Move
```

### Serializing Games

Some games are expensive to solve (not prohibitively so) and it is inconvenient to solve them each time one wants to play. To speed this up, one can define a game as an instance of `Binary` and then use `loadOrSolveTree` to load a solved game tree from a binary file or solve a tree and write a new file if one doesn't exist.

For instance:

```
-- Serialization of OneTwoTen board
instance Binary OTTBoard where
  put (OTTBoard a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (OTTBoard a b)
```



# Example Games

Some example games are included in the project inside of `Games`:

- OneTwoTen
- TicTacToe
- Connect4
- Go

# Contributors

- Ceasar Bautista (ceasarb@seas.upenn.edu)
- Zach Wasserman (zwass@seas.upenn.edu)
