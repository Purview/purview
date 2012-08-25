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

import Data.Array.Repa (Source(..), D, Shape(..))
import qualified Data.Array.Repa as Repa

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Graphics.Forensics.Algorithms.DFT
import Graphics.Forensics.Algorithms.Fragmentize
import Graphics.Forensics.Algorithms.Convolve

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
