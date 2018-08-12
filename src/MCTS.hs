module MCTS where

data Playout s a p
    = Playout
    { nWins  :: Int
    , nPlays :: Int
    , state  :: s
    }

data Tree a = Tree a [Tree a]
