-- | Provides algorithms to manipulate matrices
module Graphics.Forensics.Algorithms
       ( -- * Convolution
         OutOfRangeMode(..)
       , convolve
       , convolveS
       , stencil2
       , makeStencil2
       , PC5
       , Stencil (..)
         -- * Discrete Fourier Transform
       , idftP
       , idftS
       , dftP
       , dftS
       , calcRootsOfUnityP
       , calcRootsOfUnityS
       , calcInverseRootsOfUnityP
       , calcInverseRootsOfUnityS
       , fft1dP
       , fft2dP
       , fft3dP
       , Mode (..)
       , realToComplex
       , complexRound
         -- * Fragmentize
       , fragmentMap
         -- * Normalization
       , scaleArray
       , normalize
       ) where

import Prelude hiding (lookup)

import qualified Data.Array.Repa.Algorithms.Convolve as Repa

import Data.Array.Repa (Array(..), Z(..), DIM2, (:.)(..),
                        Source(..), D, Shape(..), (!))
import Data.Array.Repa.Repr.Unboxed (U, Unbox)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Graphics.Forensics.Algorithms.DFT

-- | Provides the different edge case handling methods for convolution
data OutOfRangeMode a = Clamp | Value a | Function (Repa.GetOut a)

-- | Convolves a matrix using the specified kernel and edge handling method
convolve :: (Unbox n, Num n, Monad m) =>
            OutOfRangeMode n -> Array U DIM2 n ->
            Array U DIM2 n -> m (Array U DIM2 n)
convolve (Clamp)      = Repa.convolveOutP Repa.outClamp
convolve (Value a)    = Repa.convolveOutP $ Repa.outAs a
convolve (Function f) = Repa.convolveOutP f

-- | Convolves a matrix using Repa's built in stencil functions
convolveS :: (Source r1 a) => OutOfRangeMode a ->
             Stencil DIM2 a -> Array r1 DIM2 a -> Array PC5 DIM2 a
convolveS (Clamp)      = mapStencil2 BoundClamp
convolveS (Value a)    = mapStencil2 (BoundConst a)
convolveS (Function _) = undefined

-- | Splits the image into NxM overlapping fragments, then maps a
--   function over each fragment to return a new 2D array
fragmentMap :: (Repa.Elt a, Num a, Repa.Elt b, Unbox a, Unbox b,
                Source r1 a) =>
               ((Repa.Array D DIM2 a) -> b) -> DIM2 ->
               Repa.Array r1 DIM2 a -> Repa.Array D DIM2 b
fragmentMap f fragmentSize array =
  Repa.traverse array newSh traverseFunc
  where
    (Z :. sx :. sy) =
      fragmentSize
    newSh (Z :. x :. y) =
      (Z :. x - sx :. y - sy)
    traverseFunc _ ix =
      f $ getArrayFragment fragmentSize ix array

getArrayFragment :: (Source r1 e) => DIM2 -> DIM2 ->
                    Repa.Array r1 DIM2 e -> Repa.Array D DIM2 e
getArrayFragment s (Z :. offsetX :. offsetY) arr =
  Repa.fromFunction s (applyOffset)
  where
    applyOffset (Z :. ix :. iy) = arr ! (Z :. ix + offsetX :. iy + offsetY)

scaleArray :: (Source r1 e, Shape sh, Real e) =>
              e -> e -> e -> e ->
             Repa.Array r1 sh e -> Repa.Array D sh Float
scaleArray oMin oMax scMin scMax image =
  Repa.map norm image
  where
    norm value = (realToFrac (value - oMin) / realToFrac oMax) *
                 realToFrac scMax + realToFrac scMin

normalize :: (Source r1 e, Shape sh, Real e) =>
             Repa.Array r1 sh e -> Repa.Array D sh Float
normalize a =
  scaleArray aMin aMax 0 1 a
  where
    aMin = minimum $ Repa.toList a
    aMax = maximum $ Repa.toList a
