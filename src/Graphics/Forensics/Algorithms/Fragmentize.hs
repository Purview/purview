module Graphics.Forensics.Algorithms.Fragmentize
       ( fragmentMap
       ) where

import Data.Array.Repa (Array(..), computeS, D, DIM2,
                        Source(..), U, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Unsafe as Repa
import qualified Data.Array.Repa.Eval as Repa
import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Array.Repa.Algorithms.Convolve as Repa

-- | Splits the image into NxM overlapping fragments, then maps a
--   function over each fragment to return a new 2D array.
fragmentMap :: (Repa.Elt a, Num a, Repa.Elt b, Unbox a, Unbox b) =>
               ((Repa.Array D DIM2 a) -> b) -> DIM2 ->
               Repa.Array U DIM2 a -> Repa.Array D DIM2 b
fragmentMap f fragmentSize !array =
  Repa.unsafeTraverse extended newSh traverseFunc
  where
    extended = extendArray fragmentSize array
    (Z :. sx :. sy) =
      fragmentSize
    newSh (Z :. x :. y) =
      (Z :. x - sx :. y - sy)
    {-# INLINE traverseFunc #-}
    traverseFunc _ ix =
      f $ getArrayFragment fragmentSize ix extended

{-# INLINE extendArray #-}
extendArray :: (Unbox e) => DIM2 -> Array U DIM2 e -> Array U DIM2 e
extendArray fragmentSize !array =
  computeS $ Repa.unsafeTraverse array newSh fillArrayElems
  where
    imgSize = extent array
    (Z :. fragX :. fragY) = fragmentSize
    xOffset = fragX `div` 2
    yOffset = fragY `div` 2
    newSh (Z :. iw :. ih) = (Z :. iw + fragX :. ih + fragY)
    {-# INLINE fillArrayElems #-}
    fillArrayElems get (Z :. ix :. iy) =
      Repa.outClamp get imgSize (Z :. ix - xOffset :. iy - yOffset)

{-# INLINE getArrayFragment #-}
getArrayFragment :: (Source r1 e) => DIM2 -> DIM2 ->
                    Array r1 DIM2 e -> Array D DIM2 e
getArrayFragment s (Z :. offsetX :. offsetY) !arr =
  Repa.fromFunction s applyOffset
  where
    {-# INLINE applyOffset #-}
    applyOffset (Z :. ix :. iy) =
      arr `unsafeIndex` (Z :. ix + offsetX :. iy + offsetY)