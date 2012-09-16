module Graphics.Forensics.Algorithms.Interpolate
       (
         interpolate2D
       ) where

import Data.Array.Repa (Array(..), D, DIM2, extent, Source(..), Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa

interpolate2D :: forall e. RealFrac e => Source r1 e =>
                 DIM2 -> Array r1 DIM2 e -> Array D DIM2 e
interpolate2D newSh !array =
  Repa.traverse array (const newSh) traverseFunc
  where
    (Z :. x  :. y) = extent array
    (Z :. x' :. y') = newSh
    sx :: e = (realToFrac x) / (realToFrac x')
    sy :: e = (realToFrac y) / (realToFrac y')
    {-# INLINE traverseFunc #-}
    traverseFunc :: (DIM2 -> e) -> DIM2 -> e
    traverseFunc get (Z :. ix :. iy) =
      (1 - frac_y) * ((1 - frac_x) * p1 + frac_x * p2) +
      frac_y * ((1 - frac_x) * p3 + frac_x * p4)
      where
        fx :: Int = floor $ sx * (realToFrac ix)
        fy :: Int = floor $ sy * (realToFrac iy)
        cx :: Int = if (fx + 1 >= x) then fx else fx + 1
        cy :: Int = if (fy + 1 >= y) then fy else fy + 1
        frac_x :: e = (realToFrac x * realToFrac sx) - realToFrac fx
        frac_y :: e = (realToFrac y * realToFrac sy) - realToFrac fy
        p1 = get (Z :. fx :. fy)
        p2 = get (Z :. cx :. fy)
        p3 = get (Z :. fx :. cy)
        p4 = get (Z :. cx :. cy)