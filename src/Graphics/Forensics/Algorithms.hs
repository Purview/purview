module Graphics.Forensics.Algorithms where

import Data.Colour.SRGB
import Graphics.Forensics.Image
import Data.Array
import Data.Array.Repa.Index
import qualified Data.Array.Repa as Repa
import qualified DSP.Convolution as DSP

class (Repa.Shape s, Ix i) => RepaArray s i where
  toArray :: Repa.Elt a => Repa.Array s a -> Array i a
  fromArray :: Repa.Elt a => Array i a -> Repa.Array s a

instance RepaArray (Z :. Int) Int where
  toArray a@ (Repa.Array (Z :. x) _) =
    listArray (1, x) (Repa.toList a)
  fromArray a =
    Repa.fromList (Z :. i) as where
      (_, i) = bounds a
      as     = elems a

instance RepaArray (Z :. Int :. Int) (Int, Int) where
  toArray a@ (Repa.Array (Z :. x :. y) _) =
    listArray ((1, 1), (x, y)) (Repa.toList a)
  fromArray a =
    Repa.fromList (Z :. i :. j) as where
      (_, (i, j)) = bounds a
      as          = elems a

instance RepaArray (Z :. Int :. Int :. Int) (Int, Int, Int) where
  toArray a@ (Repa.Array (Z :. x :. y :. z) _) =
    listArray ((1, 1, 1), (x, y, z)) (Repa.toList a)
  fromArray a =
    Repa.fromList (Z :. i :. j :. k) as where
      (_, (i, j, k)) = bounds a
      as             = elems a

convolve :: Repa.Elt a => Array (x, y) a -> Image a -> Image a
convolve kernel =
    mergeChannels .
    fromArray .
    mergeTo3DArray .
    map (`DSP.conv` kernel) .
    split3DArray .
    toArray .
    splitChannels

split3DArray :: Array (x, y, z) a -> [Array (x, y) a]
split3DArray arr =
  map f [1..3]
  where
    f x = array ((1, 1), (i, j)) (filter (f' x) as)
    f' ((_, _, z), _) = (z ==)
    as = assocs arr
    (_, (i, j, _)) = bounds arr

mergeTo3DArray :: [Array (x, y) a] -> Array (x, y, z) a
mergeTo3DArray arrs =
  listArray ((1, 1, 1), (i, j, 3)) (concatMap elems arrs) where
    (_, (i, j)) = bounds . head $ arrs