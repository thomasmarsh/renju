{-# LANGUAGE DataKinds #-}
module TestShift (testShifts) where

import BitBoard    ( BitBoard(..)
                   , Direction(..)
                   , shiftD
                   )
import Data.Digits (unDigits)

type FourByFour = BitBoard 4 4 Int

a,n,e,s,w,ne,nw,se,sw :: [Int]
a = [ 1, 1, 0, 1
    , 0, 0, 1, 0
    , 1, 0, 1, 0
    , 0, 0, 1, 1 ]

n = [ 0, 0, 1, 0
    , 1, 0, 1, 0
    , 0, 0, 1, 1
    , 0, 0, 0, 0 ]

s = [ 0, 0, 0, 0
    , 1, 1, 0, 1
    , 0, 0, 1, 0
    , 1, 0, 1, 0 ]

e = [ 0, 1, 1, 0
    , 0, 0, 0, 1
    , 0, 1, 0, 1
    , 0, 0, 0, 1 ]

w = [ 1, 0, 1, 0
    , 0, 1, 0, 0
    , 0, 1, 0, 0
    , 0, 1, 1, 0 ]

ne = [ 0, 0, 0, 1
     , 0, 1, 0, 1
     , 0, 0, 0, 1
     , 0, 0, 0, 0 ]

nw = [ 0, 1, 0, 0
     , 0, 1, 0, 0
     , 0, 1, 1, 0
     , 0, 0, 0, 0 ]

se = [ 0, 0, 0, 0
     , 0, 1, 1, 0
     , 0, 0, 0, 1
     , 0, 1, 0, 1 ]

sw = [ 0, 0, 0, 0
     , 1, 0, 1, 0
     , 0, 1, 0, 0
     , 0, 1, 0, 0 ]

pairs :: [(Direction, [Int])]
pairs = [ (N, n) , (E, e)
        , (S, s) , (W, w)
        , (NE, ne) , (NW, nw)
        , (SE, se) , (SW, sw) ]

testShifts :: IO ()
testShifts = do
    let input = BitBoard $ unDigits 2 a :: FourByFour
    putStrLn "Input:"
    print input
    mapM_ (\(dir, x) -> do
        let expected = BitBoard $ unDigits 2 x :: FourByFour
        let result = shiftD dir input

        putStrLn "--"
        putStr $ show dir ++ ":"

        if result == expected
            then putStrLn "OK"
            else do
                putStrLn ""
                putStrLn "Result:"
                print result
                putStrLn "Expected:"
                print expected
        ) pairs
