{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module State
    ( State(..)
    , Winner(..)
    ) where

data Winner a = InProgress | Draw | Win a deriving (Eq, Show)

class (Eq a, Eq (Player a), Eq (Action a)) => State a where
    -- |You must provide your own Player type
    data Player a :: *

    -- |You must provide your own action type
    data Action a :: *

    -- |Returns a representation of the starting state of the game.
    start :: a

    -- |Takes the game state and returns the current player
    currentTurn :: a -> Player a

    -- |Takes the game state, and the move to be applied.
    -- Returns the new state.
    nextState :: a -> Action a -> a

    -- |Takes a sequence of game states representing the full
    -- reverse game history, and returns the full list of moves that
    -- are legal plays for the current player.
    legalActions :: [a] -> [Action a]

    -- |Takes a sequence of game states representing the full
    -- game history.  If the game is now won, return the player.
    -- If the game is still ongoing, return Partial.  If the game
    -- is tied, return Tied.
    winner :: [a] -> Winner (Player a)
