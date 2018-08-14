module TestShift (testShifts) where

import BitBoard    ( Size(..)
                   , shiftN
                   , shiftE
                   , shiftS
                   , shiftW
                   , shiftNE
                   , shiftNW
                   , shiftSE
                   , shiftSW
                   , showBin'
                   , unMask
                   , wallN
                   , wallE
                   , wallS
                   , wallW
                   , wallNE
                   , wallNW
                   , wallSE
                   , wallSW
                   , clip'
                   )
import Data.Bits   (complement, shift, (.&.))
import Data.Digits (unDigits)

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

wallN', wallE', wallS', wallW' :: Size -> Int
wallN' = unMask . wallN
wallE' = unMask . wallE
wallS' = unMask . wallS
wallW' = unMask . wallW

wallNE', wallNW', wallSE', wallSW' :: Size -> Int
wallNE' = unMask . wallNE
wallNW' = unMask . wallNW
wallSE' = unMask . wallSE
wallSW' = unMask . wallSW

pairs :: [(String, [Int], Size -> Int, Size -> Int)]
pairs = [ ("N",  n,  shiftN,  wallS')
        , ("S",  s,  shiftS,  wallN')
        , ("E",  e,  shiftE,  wallW')
        , ("W",  w,  shiftW,  wallE')
        , ("NE", ne, shiftNE, wallSW')
        , ("NW", nw, shiftNW, wallSE')
        , ("SE", se, shiftSE, wallNW')
        , ("SW", sw, shiftSW, wallNE')
        ]

size :: Size
size = Size (4,4)

doShift :: (Size -> Int) -> Int -> Int
doShift f x = clip' size $ x `shift` f size

dump :: String -> Int -> IO ()
dump label value = do
    putStrLn $ label ++ ":"
    putStrLn (showBin' size value)

testShifts :: IO ()
testShifts = do
    putStrLn "Input:"
    putStrLn $ showBin' size (unDigits 2 a)
    mapM_ (\(label, x, f, wallF) -> do

        let shifted  = doShift f (unDigits 2 a)
        let expected = unDigits 2 x
        let wall     = wallF size
        let masked   = shifted .&. complement wall

        putStrLn "--"
        putStr $ label ++ ":"

        if masked == expected
            then putStrLn "OK"
            else do
                putStrLn ""
                dump "Result"   masked
                dump "Shifted"  shifted
                dump "Wall"     wall
                dump "Expected" expected
        ) pairs
