module Graphics.Forensics.Algorithms.Convolve
       ( convolve
       , convolveS
       , OutOfRangeMode(..)
       , outOfRangeFunction
       ) where

import Data.Array.Repa (Array(..), DIM2, Source(..))
import Data.Array.Repa.Repr.Unboxed (U, Unbox)
import qualified Data.Array.Repa.Algorithms.Convolve as Repa

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

-- | Provides the different edge case handling methods for algorithms
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
