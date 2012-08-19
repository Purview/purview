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

import Data.Array.Repa (Array(..), DIM2, Source(..), D, Shape(..))
import Data.Array.Repa.Repr.Unboxed (U, Unbox)
import qualified Data.Array.Repa as Repa

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Graphics.Forensics.Algorithms.DFT
import Graphics.Forensics.Algorithms.Fragmentize

-- | Provides the different edge case handling methods for convolution
data OutOfRangeMode a = Clamp | Value a | Function (Repa.GetOut a)

outOfRangeFunction :: OutOfRangeMode a -> Repa.GetOut a
outOfRangeFunction Clamp = Repa.outClamp
outOfRangeFunction (Value a) = Repa.outAs a
outOfRangeFunction (Function f) = f

-- | Convolves a matrix using the specified kernel and edge handling method
convolve :: (Unbox n, Num n, Monad m) =>
            OutOfRangeMode n -> Array U DIM2 n ->
            Array U DIM2 n -> m (Array U DIM2 n)
convolve mode = Repa.convolveOutP $ outOfRangeFunction mode

-- | Convolves a matrix using Repa's built in stencil functions
convolveS :: (Source r1 a) => OutOfRangeMode a ->
             Stencil DIM2 a -> Array r1 DIM2 a -> Array PC5 DIM2 a
convolveS (Clamp)      = mapStencil2 BoundClamp
convolveS (Value a)    = mapStencil2 (BoundConst a)
convolveS (Function _) = undefined

scaleArray :: (Source r1 e, Shape sh, Real e) =>
              e -> e -> e -> e ->
             Repa.Array r1 sh e -> Repa.Array D sh Float
scaleArray oMin oMax scMin scMax !image =
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
