-- | Provides algorithms to manipulate matrices
module Graphics.Forensics.Algorithms
       ( -- * Convolution
         ConvolutionMode,
         convolve,
         -- * Discrete Fourier Transform
         Mode(..),
         realToComplexMap,
         dft1d,
         dft2d,
         dft3d
       ) where

import qualified Data.Array.Repa.Algorithms.Convolve as Repa

import qualified Data.Array.Repa.Algorithms.FFT as DFT
import qualified Data.Array.Repa.Algorithms.DFT as DFT
import Data.Array.Repa.Algorithms.FFT (Mode(..))
import Data.Array.Repa.Algorithms.Complex

import Data.Array.Repa (Z(..), DIM1, DIM2, DIM3, (:.)(..))
import qualified Data.Array.Repa as Repa

import Graphics.Forensics.Array (sliceArray, glueArrays)

-- | Provides the different edge case handling methods for convolution
data ConvolutionMode a = Clamp | Value a | Function (DIM2 -> a)

-- | Convolves a matrix using the specified kernel and edge handling method
convolve :: (Repa.Elt n, Num n) => ConvolutionMode n -> Repa.Array DIM2 n ->
 Repa.Array DIM2 n -> Repa.Array DIM2 n
convolve (Clamp)      = Repa.convolveOut Repa.outClamp
convolve (Value a)    = Repa.convolveOut (Repa.outAs a)
convolve (Function f) = Repa.convolve f

-- | Applies the discrete fourier transform to a 1-dimensional array.
-- If the array size is a power of 2, 
-- fast fourier transform is used instead of naive DFT.
dft1d :: DFT.Mode -> Repa.Array DIM1 Complex -> Repa.Array DIM1 Complex
dft1d m a
  | isPowerOf2 = DFT.fft1d m a
  | forward m  = DFT.dft a
  | otherwise  = DFT.idft a
  where
    (Z :. x)   = Repa.extent a
    isPowerOf2 = DFT.isPowerOfTwo x
    forward Forward = True
    forward Reverse = True
    forward _ = False

-- | Applies the discrete fourier transform to a 2-dimensional array.
-- If any of the array dimensions is a power of 2, 
-- fast fourier transform is used instead of naive DFT.
dft2d :: DFT.Mode -> Repa.Array DIM2 Complex -> Repa.Array DIM2 Complex
dft2d m a
  | isPowerOf2 = DFT.fft2d m a
  | otherwise  = glueArrays . map (dft1d m) $ tSlices
  where
    (Z :. x :. y) = Repa.extent a
    isPowerOf2    = all DFT.isPowerOfTwo [x, y]
    slices        = sliceArray a
    tSlices       = sliceArray transposed
    transposed    = Repa.transpose . glueArrays . map (dft1d m) $ slices

-- | Applies the discrete fourier transform to a 3-dimensional array.
-- If any of the array dimensions is a power of 2, 
-- fast fourier transform is used instead of naive DFT.
dft3d :: DFT.Mode -> Repa.Array DIM3 Complex -> Repa.Array DIM3 Complex
dft3d m a
  | isPowerOf2 = DFT.fft3d m a
  | otherwise  = glueArrays . map (dft2d m) $ slices
  where
    (Z :. x :. y :. z) = Repa.extent a
    isPowerOf2         = all DFT.isPowerOfTwo [x, y, z]
    slices             = sliceArray a

realToComplex :: (Real a) => a -> Complex
realToComplex x = (realToFrac x, 0.0)

realToComplexMap :: (Real a, Repa.Elt a, Repa.Shape sh) => Repa.Array sh a -> 
                    Repa.Array sh Complex
realToComplexMap = Repa.map realToComplex