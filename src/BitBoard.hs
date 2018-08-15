{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MagicHash             #-}

module BitBoard
    ( BitBoard(..)
    , Direction(..)
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
import Numeric         (showIntAtBase)

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits (KnownNat, natVal')

data Direction
    = N  | E  | S  | W
    | NE | NW | SE | SW
    deriving (Eq, Show, Bounded, Enum)

-- |A BitBoard is a backing type (e.g., `Word8`, `Integer`, etc.) and
-- static KnownNat parameters w for width and h for height
newtype BitBoard w h a = BitBoard a deriving Eq

size :: forall w h a. (KnownNat w, KnownNat h) => BitBoard w h a -> (Int, Int)
size x = (width x, height x)
{-# INLINE size #-}

width :: forall w h a. (KnownNat w) => BitBoard w h a -> Int
width _ = fromIntegral $ natVal' (proxy# :: Proxy# w)
{-# INLINE width #-}

height :: forall w h a. (KnownNat h) => BitBoard w h a -> Int
height _ = fromIntegral $ natVal' (proxy# :: Proxy# h)
{-# INLINE height #-}

toInt :: BitBoard w h a -> a
toInt (BitBoard m) = m

instance (KnownNat w, KnownNat h, Bits a, Num a)
    => Bits (BitBoard w h a) where
    (.&.)                 = applyBin (.&.)
    (.|.)                 = applyBin (.|.)
    xor                   = applyBin xor
    complement            = applyUn complement
    shift                 = applyS shift
    shiftL                = applyS shiftL
    shiftR                = applyS shiftR
    rotate                = applyS rotate
    rotateL               = applyS rotateL
    rotateR               = applyS rotateR
    bitSize x             = width x * height x
    bitSizeMaybe          = Just . finiteBitSize
    isSigned _            = False
    testBit (BitBoard m)  = testBit m
    popCount (BitBoard m) = popCount m
    bit i                 = BitBoard (bit i)

instance (KnownNat w, KnownNat h, Bits a, Num a)
    => FiniteBits (BitBoard w h a) where
    finiteBitSize x = width x * height x

-- The following methods naively assume that any operation they are
-- applying to the bits could cause overflow and that the result
-- must be masked within range. This produces excess AND steps. Any
-- code that needs better performance should operate on the underlying
-- raw values.

applyBin
    :: (KnownNat w, KnownNat h, Bits a, Num a)
    => (a -> a -> a)
    -> BitBoard w h a
    -> BitBoard w h a
    -> BitBoard w h a
applyBin f b@(BitBoard m1)
             (BitBoard m2)
    = BitBoard . maskToSize b $ f m1 m2
{-# INLINE applyBin #-}

applyUn :: (KnownNat w, KnownNat h, Bits a, Num a)
        => (a -> a) -> BitBoard w h a -> BitBoard w h a
applyUn f b@(BitBoard m) = BitBoard . maskToSize b $ f m
{-# INLINE applyUn #-}

applyS :: (KnownNat w, KnownNat h, Bits a, Num a)
       => (a -> b -> a) -> BitBoard w h a -> b -> BitBoard w h a
applyS f b@(BitBoard m) x = BitBoard . maskToSize b $ f m x
{-# INLINE applyS #-}

maskToSize :: (KnownNat w, KnownNat h, Bits a, Num a)
           => BitBoard w h a -> a -> a
maskToSize b m = m .&. (2 ^ finiteBitSize b - 1)
{-# INLINE maskToSize #-}

instance (KnownNat h, KnownNat w, Bits a, Integral a, Show a)
    => Show (BitBoard w h a) where

    show b@(BitBoard m)
        = unlines . map (intersperse ' ') $ rows
        where binStr  = showIntAtBase 2 intToDigit m ""
              missing = finiteBitSize b - length binStr
              pad     = replicate missing '0'
              rows    = chunksOf (width b) (pad ++ binStr)

-- |A row of 1s along each wall
wall :: (KnownNat w, KnownNat h, Bits a, Num a)
     => Direction -> (Int, Int) -> BitBoard w h a
wall N (w,h) = BitBoard ((2^w-1) `shift` (w * (h-1)))
wall E (w,h) = BitBoard (foldl (.|.) 0
                               [ 1 `shift` (w*x)
                               | x<-[0..h-1] ])
wall S (w,_) = BitBoard (2^w-1)
wall W (w,h) = BitBoard (foldl (.|.) 0
                               [ 1 `shift` (w*x+w-1)
                               | x <- [0..h-1] ])

wall NE z = wall N z .|. wall E z
wall NW z = wall N z .|. wall W z
wall SE z = wall S z .|. wall E z
wall SW z = wall S z .|. wall W z

shiftD' :: (KnownNat w, KnownNat h, Bits a, Num a)
        => BitBoard w h a -> Direction -> Int -> BitBoard w h a
shiftD' b dir amount
    = (b `shift` amount)
    .&. complement (wall dir (size b))

shiftD :: (KnownNat w, KnownNat h, Bits a, Num a)
       => Direction -> BitBoard w h a -> BitBoard w h a
shiftD N  b = shiftD' b S  (width b)
shiftD E  b = shiftD' b W  (-1)
shiftD S  b = shiftD' b N  (-(width b))
shiftD W  b = shiftD' b E  1
shiftD NE b = shiftD' b SW (width b-1)
shiftD NW b = shiftD' b SE (width b+1)
shiftD SE b = shiftD' b NW (-(width b)-1)
shiftD SW b = shiftD' b NE (-(width b)+1)

erode :: (KnownNat w, KnownNat h, Bits a, Num a)
      => Direction -> BitBoard w h a -> BitBoard w h a
erode dir b = b .&. shiftD dir b

{-
dilate :: (KnownNat w, KnownNat h, Bits a, Num a)
       => Direction -> BitBoard w h a -> BitBoard w h a
dilate dir b = b .|. shiftD dir b
-}

-- |Iterate over a function k times
applyN :: (a -> a) -> Int -> a -> a
applyN f k x = iterate f x !! k

-- |Returns True if there are k-in-a-row bits in any direction, orthogonal
-- or diagonal.
hasNInRow :: (KnownNat w, KnownNat h, Bits a, Integral a)
          => BitBoard w h a -> Int -> Bool
hasNInRow b k = any (/= 0) eroded
    where
        dirs = [minBound..]
        eroded = map (\dir -> toInt $ applyN (erode dir) (k-1) b) dirs
