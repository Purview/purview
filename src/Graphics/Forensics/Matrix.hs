module Graphics.Forensics.Matrix where

import Data.Array as Array
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Word

-- | Converts a matrix of floats between 0 and 1 to a matrix of bytes
floatToByteMatrix :: Repa.Shape s => Repa.Array s Float -> Repa.Array s Word8
floatToByteMatrix = Repa.map floatToByte

-- | Converts a matrix of bytes to a matrix of floats between 0 and 1
byteToFloatMatrix :: Repa.Shape s => Repa.Array s Word8 -> Repa.Array s Float
byteToFloatMatrix = Repa.map byteToFloat

{-# INLINE byteToFloat #-}
byteToFloat :: Word8 -> Float
byteToFloat = (/ 255) . fromIntegral

{-# INLINE floatToByte #-}
floatToByte :: Float -> Word8
floatToByte = round . (* 255) . clamp 0 1

class (Repa.Shape s, Array.Ix i) => RepaArray s i where
  toArray :: Repa.Elt a => Repa.Array s a -> Array.Array i a

instance RepaArray (Z :. Int) Int where
  toArray a @ (Repa.Array (Z :. x) _) =
    Array.listArray (0, x) (Repa.toList a)

instance RepaArray (Z :. Int :. Int) (Int, Int) where
  toArray a @ (Repa.Array (Z :. x :. y) _) =
    listArray ((0, 0), (x, y)) (Repa.toList a)

instance RepaArray (Z :. Int :. Int :. Int) (Int, Int, Int) where
  toArray a @ (Repa.Array (Z :. x :. y :. z) _) =
    listArray ((0, 0, 0), (x, y, z)) (Repa.toList a)

{-# INLINE clamp #-}
clamp :: Ord n => n -> n -> n -> n
clamp low hi num
  | num < low = low
  | num > hi  = hi
  | otherwise = num
