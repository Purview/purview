-- | Provides various 'Array' manipulation tools.
module Graphics.Forensics.Array
       ( -- * Array
         -- ** Formats
         floatToByteArray
       , byteToFloatArray
       , floatToByte
       , byteToFloat
         -- ** Slices
       , sliceArray
       , glueArrays
       , getArraySlice
         -- ** Conversion
       , MatchingShape(..)
       ) where

import Data.Array as Array
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.List
import Data.Word

-- | Converts an array of floats between 0 and 1 to an array of bytes
floatToByteArray :: Repa.Shape s => Repa.Array s Float -> Repa.Array s Word8
floatToByteArray = Repa.map floatToByte

-- | Converts an array of bytes to an array of floats between 0 and 1
byteToFloatArray :: Repa.Shape s => Repa.Array s Word8 -> Repa.Array s Float
byteToFloatArray = Repa.map byteToFloat

-- | Converts a byte to a float between 0 and 1
byteToFloat :: Word8 -> Float
byteToFloat = (/ 255) . fromIntegral
{-# INLINE byteToFloat #-}

-- | Converts a float between 0 and 1 to a byte
floatToByte :: Float -> Word8
floatToByte = round . (* 255) . clamp 0 1
{-# INLINE floatToByte #-}

-- | Combines a series of equally shaped arrays into an array of one
-- more dimension. The last dimension specifies the index of the
-- source array.
glueArrays :: (Repa.Elt a, Repa.Shape sh)
               => [Repa.Array sh a] -> Repa.Array (sh :. Int) a
glueArrays =
  Repa.force . foldl1' Repa.append . map makeFlatArray
  where
    makeFlatArray arr @ (Repa.Array sh _) = Repa.reshape (sh :. 1) arr

-- | Slices an array into multiple arrays of one dimension lower.
sliceArray :: (Repa.Elt a, Repa.Shape sh)
                => Repa.Array (sh :. Int) a -> [Repa.Array sh a]
sliceArray arr @ (Repa.Array (_ :. slices) _) =
  map (flip getArraySlice arr) [0..slices - 1]

-- | Get the array slice with the given number from the given array.
getArraySlice :: (Repa.Elt a, Repa.Shape sh)
                   => Int -> Repa.Array (sh :. Int) a -> Repa.Array sh a
getArraySlice n arr = Repa.slice arr $ Repa.Any :. n

-- | A relationship between two index types specifying that they
-- describe the same shape.
class (Repa.Shape s, Ix i) => MatchingShape s i where
  -- | Converts a 'Data.Array.Repa.Array' into a 'Data.Array.Array'
  toDataArray :: Repa.Elt a => Repa.Array s a -> Array i a
  -- | Converts a 'Data.Array.Array' into a 'Data.Array.Repa.Array'
  fromDataArray :: Repa.Elt a => Array i a -> Repa.Array s a

instance MatchingShape (Z :. Int) Int where
  toDataArray a @ (Repa.Array (Z :. x) _) =
    listArray b $ Repa.toList a
    where
      b = (0, x - 1)
  fromDataArray a =
    Repa.fromList shape $ elems a
    where
      shape    = Z :. (x1 - x0 + 1)
      (x0, x1) = bounds a

instance MatchingShape (Z :. Int :. Int) (Int, Int) where
  toDataArray a @ (Repa.Array (Z :. x :. y) _) =
    listArray b $ Repa.toList a
    where
      b = ((0, 0), (x - 1, y - 1))
  fromDataArray a =
    Repa.fromList shape $ elems a
    where
      shape = Z :. (x1 - x0 + 1) :. (y1 - y0 + 1)
      ((x0, y0), (x1, y1)) = bounds a

instance MatchingShape (Z :. Int :. Int :. Int) (Int, Int, Int) where
  toDataArray a @ (Repa.Array (Z :. x :. y :. z) _) =
    listArray b $ Repa.toList a
    where
      b = ((0, 0, 0), (x - 1, y - 1, z - 1))
  fromDataArray a =
    Repa.fromList shape $ elems a
    where
      shape = Z :. (x1 - x0 + 1) :. (y1 - y0 + 1) :. (z1 - z0 + 1)
      ((x0, y0, z0), (x1, y1, z1)) = bounds a

{-# INLINE clamp #-}
clamp :: Ord n => n -> n -> n -> n
clamp low hi num
  | num < low = low
  | num > hi  = hi
  | otherwise = num
