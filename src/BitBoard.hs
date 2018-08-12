module BitBoard
    ( Mask(..)
    , Size(..)
    , Coord(..)
    , clip
    , clip'
    , dilate
    , dilateOrtho
    , erode
    , erodeOrtho
    , hasNInRow
    , isSet
    , printMask'
    , set
    , shiftN
    , shiftE
    , shiftS
    , shiftW
    , showBin
    , showBin'
    , showMask
    )
    where

import Data.Bits       ((.&.), (.|.), Bits, setBit, shift, testBit)
import Data.Char       (intToDigit)
import Data.List       (intersperse)
import Data.List.Split (chunksOf)
import Data.Tuple      (swap)
import Numeric         (showIntAtBase)

newtype Mask a = Mask a           deriving (Eq, Show)
newtype Size   = Size (Int, Int)  deriving (Eq, Show)
newtype Coord  = Coord (Int, Int) deriving (Eq, Show)
newtype Index  = Index Int        deriving (Eq, Show)

unMask :: Mask a -> a
unMask (Mask m) = m

showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 intToDigit n ""

showBin' :: (Integral a, Show a) => Size -> a -> String
showBin' sz n = (unlines . map (intersperse ' ')) rows
    where binStr = showBin n
          missing = numPositions - length binStr
          pad = replicate missing '0'
          rows = chunksOf width (reverse $ pad ++ binStr)
          Size (width, height) = sz
          numPositions = width * height

-- |Since Integers do not truncate overflow, `clip` can be used to
-- mask the number within range
clip' :: (Bits a, Num a) => Size -> a -> a
clip' (Size (w, h)) m = m .&. (2^(w*h)-1)

-- |Calls `clip'` for values wrapped in Mask type
clip :: (Bits a, Num a) => Size -> Mask a -> Mask a
clip sz (Mask m) = Mask $ clip' sz m

-- |These return a number bits to shift for a given direction
shiftN, shiftE, shiftS, shiftW :: Size -> Int
shiftN (Size (w,_))  =  w
shiftE _             =  1
shiftS (Size (w,_))  = -w
shiftW _             = -1

shiftNE, shiftSE, shiftNW, shiftSW :: Size -> Int
shiftNE (Size (w,_)) =  w+1
shiftSE (Size (w,_)) = -w+1
shiftNW (Size (w,_)) =  w-1
shiftSW (Size (w,_)) = -w-1

erode :: (Bits a, Num a) => (Size -> Int) -> Size -> Mask a -> Mask a
erode f sz (Mask m) = Mask (m .&. clip' sz (m `shift` f sz))

erodeOrtho :: (Bits a, Num a) => Size -> Mask a -> Mask a
erodeOrtho sz (Mask m)
    = Mask $ m
    .&. clip' sz (m `shift` shiftN sz)
    .&. clip' sz (m `shift` shiftE sz)
    .&.          (m `shift` shiftS sz)
    .&.          (m `shift` shiftW sz)

dilate :: (Bits a, Num a) => (Size -> Int) -> Size -> Mask a -> Mask a
dilate f sz (Mask m) = Mask (m .|. clip' sz (m `shift` f sz))

dilateOrtho :: (Bits a, Num a) => Size -> Mask a -> Mask a
dilateOrtho sz (Mask m)
    = Mask $ m
    .|. clip' sz (m `shift` shiftN sz)
    .|. clip' sz (m `shift` shiftE sz)
    .|.          (m `shift` shiftS sz)
    .|.          (m `shift` shiftW sz)

hasNInRow :: (Bits a, Num a) => Size -> Mask a -> Int -> Bool
hasNInRow sz m k
    = any (/= 0) eroded
    where
        erodeMany f = iterate (erode f sz) m !! (k-1)
        directions  = [ shiftN, shiftE, shiftS, shiftW
                      , shiftNE, shiftSE, shiftNW, shiftSW]
        eroded      = map (unMask . erodeMany) directions

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


