{-# LANGUAGE TypeFamilies #-}

module Game.TTT where

import qualified Data.Set as S
import           State (State(..), Winner(..))

-- Tic-tac-toe example

newtype TTTState = TTTState (S.Set (Action TTTState)) deriving (Eq, Show)

allActions :: S.Set (Action TTTState)
allActions = S.fromList [P0, P1, P2, P3, P4, P5, P6, P7, P8]

instance State TTTState where
    data Player TTTState = X | O deriving (Eq, Show)

    data Action TTTState
        = P0 | P1 | P2
        | P3 | P4 | P5
        | P6 | P7 | P8
        deriving (Eq, Ord, Show)

    start = TTTState S.empty

    currentTurn (TTTState s) = if even (S.size s) then X else O

    nextState (TTTState s) a = TTTState (S.insert a s)

    legalActions [] = S.toList allActions
    legalActions (TTTState s:_) = S.toList $ allActions `S.difference` s

    winner _ = Win X
