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
    , shiftMaskN
    , shiftMaskE
    , shiftMaskS
    , shiftMaskW
    , shiftN
    , shiftE
    , shiftS
    , shiftW
    , shiftNE
    , shiftNW
    , shiftSE
    , shiftSW
    , showBin
    , showBin'
    , showMask
    , unMask
    , wallN
    , wallE
    , wallS
    , wallW
    , wallNE
    , wallNW
    , wallSE
    , wallSW
    )
    where

import Data.Bits       ( (.&.)
                       , (.|.)
                       , Bits
                       , complement
                       , setBit
                       , shift
                       , testBit)
import Data.Char       (intToDigit)
import Data.List       (intersperse)
import Data.List.Split (chunksOf)
import Data.Tuple      (swap)
import Numeric         (showIntAtBase)

data Direction = N | E | S | W | NE | NW | SE | SW deriving (Eq, Show)

newtype Mask a = Mask a           deriving (Eq, Show)
newtype Size   = Size (Int, Int)  deriving (Eq, Show)
newtype Coord  = Coord (Int, Int) deriving (Eq, Show)
newtype Index  = Index Int        deriving (Eq, Show)

-- TODO: do we need a Bits instance for Mask?
-- We might include the precision as a parameter so
-- we can do the appropriate masking of overflow bits.
-- This would be equivalent to C++'s std::bitset<size_t>.
--
-- E.g.:
--
--  newtype BitSet a = BitSet (a, Int)
--
-- A simple implementation might decide that only BitSets
-- of like precisions can be operated on.

unMask :: Mask a -> a
unMask (Mask m) = m
{-# INLINE unMask #-}

showBin :: (Integral a, Show a) => a -> String
showBin n = showIntAtBase 2 intToDigit n ""

showBin' :: (Integral a, Show a) => Size -> a -> String
showBin' sz n = (unlines . map (intersperse ' ')) rows
    where binStr = showBin n
          missing = numPositions - length binStr
          pad = replicate missing '0'
          -- rows = chunksOf width (reverse $ pad ++ binStr)
          rows = chunksOf width (pad ++ binStr)
          Size (width, height) = sz
          numPositions = width * height

-- |Since Integers do not truncate overflow, `clip` can be used to
-- mask the number within range
clip' :: (Bits a, Num a) => Size -> a -> a
clip' (Size (w, h)) m = m .&. (2^(w*h)-1)

nilClip' :: Size -> a -> a
nilClip' _ m = m


-- |Calls `clip'` for values wrapped in Mask type
clip :: (Bits a, Num a) => Size -> Mask a -> Mask a
clip sz (Mask m) = Mask $ clip' sz m

-- |A row of 1s along each wall
-- |This is just wallS left shifted by number of columns
wallN, wallE, wallS, wallW :: (Bits a, Num a) => Size -> Mask a
wallN (Size (mx,my)) = Mask $ (2^mx-1) `shift` (mx * (my-1))
-- |Sets ever rows's low bit
wallW (Size (mx,my)) = Mask (foldl (.|.) 0 [1 `shift` (mx*x+mx-1) | x<-[0..my-1]])
-- |Sets all low bit 1s
wallS (Size (mx,_)) = Mask $ 2^mx-1
-- |Left shifted wallE
wallE (Size (mx,my)) = Mask (foldl (.|.) 0 [1 `shift` (mx*x) | x<-[0..my-1]])

wallNE, wallNW, wallSE, wallSW :: (Bits a, Num a) => Size -> Mask a
wallNE sz = Mask (n .|. e)
    where Mask n = wallN sz
          Mask e = wallE sz
wallNW sz = Mask (n .|. w)
    where Mask n = wallN sz
          Mask w = wallW sz
wallSE sz = Mask (s .|. e)
    where Mask s = wallS sz
          Mask e = wallE sz
wallSW sz = Mask (s .|. w)
    where Mask s = wallS sz
          Mask w = wallW sz


-- |These return a number bits to shift for a given direction
shiftN, shiftE, shiftS, shiftW :: Size -> Int
shiftN (Size (w,_))  =  w
shiftW _             =  1
shiftS (Size (w,_))  = -w
shiftE _             = -1

shiftNE, shiftNW, shiftSE, shiftSW :: Size -> Int
shiftNW (Size (w,_)) =  w+1
shiftNE (Size (w,_)) =  w-1
shiftSW (Size (w,_)) = -w+1
shiftSE (Size (w,_)) = -w-1

shiftMask :: (Bits a, Num a)
          => (Size -> Int)     -- shift amount function
          -> (Size -> Mask a)  -- wall function
          -> (Size -> a -> a)  -- clip function
          -> Size
          -> Mask a
          -> Mask a
shiftMask shiftF wallF clipF sz (Mask m)
    = Mask (shifted .&. complement wall)
    where shifted   = clipF sz $ m `shift` shiftF sz
          Mask wall = wallF sz

shiftMaskN, shiftMaskE, shiftMaskS, shiftMaskW
    :: (Bits a, Num a)
    => Size -> Mask a
    -> Mask a
shiftMaskN = shiftMask shiftN wallS clip'
shiftMaskE = shiftMask shiftE wallW clip'
shiftMaskS = shiftMask shiftS wallN nilClip'
shiftMaskW = shiftMask shiftW wallE nilClip'


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


