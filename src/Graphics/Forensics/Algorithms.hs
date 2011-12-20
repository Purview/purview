module Graphics.Forensics.Algorithms where

import Data.Array
import Data.Array.Repa.Index
import qualified Data.Array.Repa as Repa

class (Repa.Shape s, Ix i) => RepaArray s i where
  toArray :: Repa.Elt a => Repa.Array s a -> Array i a

instance RepaArray DIM1 Int where
  toArray a@ (Repa.Array (Z :. x) _) =
    listArray (1, x) (Repa.toList a)

instance RepaArray DIM2 (Int, Int) where
  toArray a@ (Repa.Array (Z :. x :. y) _) =
    listArray ((1, 1), (x, y)) (Repa.toList a)

instance RepaArray DIM3 (Int, Int, Int) where
  toArray a@ (Repa.Array (Z :. x :. y :. z) _) =
    listArray ((1, 1, 1), (x, y, z)) (Repa.toList a)