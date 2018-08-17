module Main where

import State (State(..), Winner(..))
import TestShift (testShifts)
import System.Random (RandomGen, getStdGen)
import Game.Gomoku (Gomoku(..))
import MCTS

selfPlay :: (Show a, State a, RandomGen g)
         => a -> Config a g -> Int -> g-> IO ()
selfPlay state conf n g
    | winner [state] == InProgress = do
        print state
        let (state', g') = doIterativeMcts state conf n g
        selfPlay state' conf n g'
    | otherwise = do
        print state
        pure ()

main :: IO ()
main = do
    testShifts
    g <- getStdGen
    selfPlay (start :: Gomoku) defaultConfig 40 g
