module Main where

import Game.Gomoku
import State (Winner(..))
import System.Random (randomRIO)
import TestShift (testShifts)

rstep :: Gomoku -> IO Gomoku
rstep s = do
    let actions = legalActions [s]
    n <- randomRIO (0::Int,length actions-1)
    pure $ nextState s (actions !! n)

rollout :: Gomoku -> IO Gomoku
rollout g
    = case winner [g] of
        InProgress -> do
            g' <- rstep g
            rollout g'
        _ -> pure g

main :: IO ()
main = do
    rollout (start :: Gomoku) >>= (\x
        -> do {printBoard x;  print $ winner [x]})
    testShifts
