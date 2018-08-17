{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module State
    ( State(..)
    , Winner(..)
    ) where

import System.Random (RandomGen, randomR, randomRIO)

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

    legalChildren :: a -> [a]
    legalChildren x = map (nextState x) $ legalActions [x]

    allPlayers :: [Player a]

    -- |Takes a sequence of game states representing the full
    -- game history.  If the game is now won, return the player.
    -- If the game is still ongoing, return InProgress.  If the game
    -- is tied, return Draw.
    winner :: [a] -> Winner (Player a)

    -- |A default implementation is provided for this function.
    rollout :: RandomGen g => a -> g -> (Winner (Player a), g)
    rollout x g = case winner [x] of
                        InProgress -> rollout x' g'
                        a          -> (a, g)
        where
            actions = legalActions [x]
            (action, g') = pick g actions
            x' = nextState x action

    rolloutIO :: a -> IO a
    rolloutIO x = case winner [x] of
                    InProgress -> do
                        let actions = legalActions [x]
                        n <- randomRIO (0::Int, length actions-1)
                        rolloutIO $  nextState x (actions !! n)
                    _ -> pure x

pick :: RandomGen g => g -> [a] -> (a, g)
pick _ [] = error "picking from empty list"
pick g xs = (xs !! n, g')
    where
        (n, g') = randomR (0, length xs-1) g
