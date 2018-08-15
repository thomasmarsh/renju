module BitBoard
    ( BitBoard(..)
    , Direction(..)
    , Size(..)
    , wall
    , shiftD
    , hasNInRow
    , toInt
    )
    where

import Data.Bits       (Bits(..), FiniteBits(..))
import Data.Char       (intToDigit)
import Data.List       (intersperse)
import Data.List.Split (chunksOf)
import Data.Semigroup  (Semigroup(..))
import Data.Tuple      (swap)
import Numeric         (showIntAtBase)

data Direction
    = N  | E  | S  | W
    | NE | NW | SE | SW
    deriving (Eq, Show)

newtype Size = Size (Int, Int, Bool) deriving (Eq, Show)

instance Semigroup Size where
    Size    (_,_,False) <>    Size (_,_,False) = Size (0,0,False)
    Size    (_,_,False) <> x@(Size (_,_,True)) = x
    x@(Size (_,_,True)) <>    Size (_,_,False) = x
    x@(Size (_,_,True)) <> y@(Size (_,_,True))
        | x /= y    = error $ "size mismatch: " ++ show (x, y)
        | otherwise = x

-- |A BitBoard is a backing type (e.g., `Word8`, `Integer`, etc.)
-- where a is in the context `(Num a, Bits a)`, and has a Size component.
newtype BitBoard a = BitBoard (a, Size) deriving Eq

width :: BitBoard a -> Int
width (BitBoard (_, Size(w, _, _))) = w

size :: BitBoard a -> Size
size (BitBoard (_, z)) = z

toInt :: (Integral a) => BitBoard a -> a
toInt (BitBoard (m, _)) = m

instance (Bits a, Num a) => Bits (BitBoard a) where
    (.&.)                              = applyBin (.&.)
    (.|.)                              = applyBin (.|.)
    xor                                = applyBin xor
    complement                         = applyUn complement
    shift                              = applyS shift
    shiftL                             = applyS shiftL
    shiftR                             = applyS shiftR
    rotate                             = applyS rotate
    rotateL                            = applyS rotateL
    rotateR                            = applyS rotateR
    bitSize (BitBoard (_, Size (w,h,_))) = w * h
    bitSizeMaybe                       = Just . bitSize
    isSigned _                         = False
    testBit (BitBoard (m, _))          = testBit m
    -- TODO: this `Size (0,0)` is a major issue that needs to be fixed.
    bit i                              = BitBoard (bit i, Size (0,0,False))
    popCount (BitBoard (m, _))         = popCount m

instance (Bits a, Num a) => FiniteBits (BitBoard a) where
    finiteBitSize = bitSize

-- The following methods naively assume that any operation they are
-- applying to the bits could cause overflow and that the result
-- must be masked within range. This produces excess AND steps. Any
-- code that needs better performance should operate on the underlying
-- raw values.

applyBin
    :: (Bits a, Num a)
    => (a -> a -> a)
    -> BitBoard a
    -> BitBoard a
    -> BitBoard a
applyBin f b@(BitBoard (m1, z1))
             (BitBoard (m2, z2))
    = BitBoard ( maskToSize b $ f m1 m2 , z1 <> z2 )
{-# INLINE applyBin #-}

applyUn :: (Bits a, Num a) => (a -> a) -> BitBoard a -> BitBoard a
applyUn f b@(BitBoard (m, z)) = BitBoard (maskToSize b $ f m, z)
{-# INLINE applyUn #-}

applyS :: (Bits a, Num a) => (a -> b -> a) -> BitBoard a -> b -> BitBoard a
applyS f b@(BitBoard (m, z)) x = BitBoard (maskToSize b $ f m x, z)
{-# INLINE applyS #-}

maskToSize :: (Bits a, Num a) => BitBoard a -> a -> a
maskToSize b m = m .&. (2 ^ bitSize b - 1)
{-# INLINE maskToSize #-}

instance (Bits a, Integral a, Show a) => Show (BitBoard a) where
    show b@(BitBoard (m, Size (w,_,_)))
        = unlines . map (intersperse ' ') $ rows
        where binStr  = showIntAtBase 2 intToDigit m ""
              missing = bitSize b - length binStr
              pad     = replicate missing '0'
              rows    = chunksOf w (pad ++ binStr)


-- |A row of 1s along each wall
wall :: (Bits a, Num a) => Direction -> Size -> BitBoard a
wall N z@(Size (w,h,_)) = BitBoard ((2^w-1) `shift` (w * (h-1)), z)
wall E z@(Size (w,h,_)) = BitBoard (foldl (.|.) 0
                                        [ 1 `shift` (w*x)
                                        | x<-[0..h-1] ], z)
wall S z@(Size (w,_,_)) = BitBoard (2^w-1, z)
wall W z@(Size (w,h,_)) = BitBoard (foldl (.|.) 0
                                        [ 1 `shift` (w*x+w-1)
                                        | x <- [0..h-1] ], z)

wall NE z = wall N z .|. wall E z
wall NW z = wall N z .|. wall W z
wall SE z = wall S z .|. wall E z
wall SW z = wall S z .|. wall W z

shiftD' :: (Bits a, Num a) => BitBoard a -> Direction -> Int -> BitBoard a
shiftD' b dir amount
    = (b `shift` amount)
    .&. complement (wall dir (size b) :: (Bits a, Num a) => BitBoard a)

shiftD :: (Bits a, Num a) => Direction -> BitBoard a -> BitBoard a
shiftD N  b = shiftD' b S  (width b)
shiftD E  b = shiftD' b W  (-1)
shiftD S  b = shiftD' b N  (-(width b))
shiftD W  b = shiftD' b E  1
shiftD NE b = shiftD' b SW (width b-1)
shiftD NW b = shiftD' b SE (width b+1)
shiftD SE b = shiftD' b NW (-(width b)-1)
shiftD SW b = shiftD' b NE (-(width b)-1)

erode :: (Bits a, Num a) => Direction -> BitBoard a -> BitBoard a
erode dir b = b .&. shiftD dir b

dilate :: (Bits a, Num a) => Direction -> BitBoard a -> BitBoard a
dilate dir b = b .|. shiftD dir b

-- |Iterate over a function k times
applyN :: (a -> a) -> Int -> a -> a
applyN f k x = iterate f x !! k

-- |Returns True if there are k-in-a-row bits in any direction, orthogonal
-- or diagonal.
hasNInRow :: (Bits a, Integral a) => BitBoard a -> Int -> Bool
hasNInRow b k = any (/= 0) eroded
    where
        dirs = [N,E,S,W,NE,NW,SE,SW]
        eroded = map (\dir -> toInt $ applyN (erode dir) (k-1) b) dirs
