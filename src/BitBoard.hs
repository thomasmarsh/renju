module BitBoard
    ( Mask(..)
    , Size(..)
    , Coord(..)
    , showBin
    , showBin'
    , showMask
    , printMask'
    , set
    , isSet
    )
    where

import Data.Bits       (Bits, setBit, testBit)
import Data.Char       (intToDigit)
import Data.List.Split (chunksOf)
import Data.Tuple      (swap)
import Numeric         (showIntAtBase)

newtype Mask a = Mask a           deriving (Eq, Show)
newtype Size   = Size (Int, Int)  deriving (Eq, Show)
newtype Coord  = Coord (Int, Int) deriving (Eq, Show)
newtype Index  = Index Int        deriving (Eq, Show)

showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 intToDigit n ""

showBin' :: (Integral a, Show a) => Size -> a -> String
showBin' sz n = unlines rows
    where binStr = showBin n
          missing = numPositions - length binStr
          pad = replicate missing '0'
          rows = chunksOf width (reverse $ pad ++ binStr)
          Size (width, height) = sz
          numPositions = width * height

showMask :: (Integral a, Show a) => Mask a -> String
showMask (Mask m) = showBin m

printMask' :: (Integral a, Show a) => Size -> Mask a -> IO ()
printMask' sz (Mask m)
    = putStrLn (showBin' sz m)

coordToIndex :: Size -> Coord -> Index
coordToIndex (Size (width, _)) (Coord (x, y))
    = Index (y * width + x)

indexToCoord :: Size -> Index -> Coord
indexToCoord (Size (width, _)) (Index i)
    = Coord (swap $ i `divMod` width)

set :: Bits a => Size -> Mask a -> Coord -> Mask a
set sz (Mask m) pos
    = Mask (m `setBit` n)
    where (Index n) = coordToIndex sz pos

isSet :: Bits a => Size -> Mask a -> Coord -> Bool
isSet sz (Mask m) pos
    = m `testBit` n
    where (Index n) = coordToIndex sz pos
