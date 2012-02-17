-- | Provides algorithms to manipulate matrices
module Graphics.Forensics.Algorithms
       ( -- * Convolution
         OutOfRangeMode(..),
         convolve,
         -- * Discrete Fourier Transform
         Mode(..),
         realToComplex,
         complexRound,
         dft1d,
         dft2d,
         center1d,
         center2d,
         -- * Fragmentize
         fragmentize,
         fragmentMap
       ) where
import Prelude hiding (lookup)
import qualified Data.Array.Repa.Algorithms.Convolve as Repa
import qualified Data.Array.Repa.Algorithms.FFT as DFT
import qualified Data.Array.Repa.Algorithms.DFT as DFT
import Data.Array.Repa.Algorithms.FFT (Mode(..))
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.Algorithms.DFT.Center
import Data.Array.Repa (Z(..), DIM1, DIM2, DIM4, All(..), (:.)(..))
import qualified Data.Array.Repa as Repa

import Graphics.Forensics.Array (sliceArray, glueArrays)

-- | Provides the different edge case handling methods for convolution
data OutOfRangeMode a = Clamp | Value a | Function (DIM2 -> a)

-- | Convolves a matrix using the specified kernel and edge handling method
convolve :: (Repa.Elt n, Num n) => OutOfRangeMode n -> Repa.Array DIM2 n ->
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
  | otherwise  = Repa.transpose .
                 glueArrays . map (dft1d m) . sliceArray .
                 Repa.transpose .
                 glueArrays . map (dft1d m) . sliceArray $ a
  where
    (Z :. x :. y) = Repa.extent a
    isPowerOf2    = all DFT.isPowerOfTwo [x, y]

realToComplex :: (Real a) => a -> Complex
realToComplex x =
  (realToFrac x, 0.0)

complexRound :: Complex -> Complex
complexRound (a, b) =
  (fromInteger $ round a, fromInteger $ round b)

fragmentize :: (Repa.Elt a, Num a) =>
               OutOfRangeMode a -> DIM2 -> Repa.Array DIM2 a
               -> Repa.Array DIM4 a
fragmentize (Clamp) kSh image @ (Repa.Array iSh _) =
  image `Repa.deepSeqArray` Repa.force $
  Repa.traverse image (shapeConv kSh) (gen Repa.outClamp iSh kSh)
fragmentize (Value x) kSh image @ (Repa.Array iSh _) =
  image `Repa.deepSeqArray` Repa.force $
  Repa.traverse image (shapeConv kSh) (gen (Repa.outAs x) iSh kSh)
fragmentize (Function _) _ _ = undefined

shapeConv :: DIM2 -> DIM2 -> DIM4
shapeConv (Z :. x3 :. x4) sh =
  sh :. x3 :. x4

gen :: (Repa.Elt a) => Repa.GetOut a -> DIM2 -> DIM2 ->
       (DIM2 -> a) -> DIM4 -> a
gen getOut imgSh @(_ :. iH :. iW)
  (_ :. kH :. kW) lookup (Z :. x :. y :. kx :. ky) =
  lookup' (Z :. (x - kH2 + kx) :. (y - kW2 + ky))
  where
    !kH2 = kH `div` 2
    !kW2 = kW `div` 2
    !borderL = kW2
    !borderR = iW  - kW2  - 1
    !borderU = kH2
    !borderD = iH - kH2 - 1
    lookup'  ix @ (_ :. j :. i)
      | i < borderL = getOut lookup imgSh ix
      | i > borderR = getOut lookup imgSh ix
      | j < borderU = getOut lookup imgSh ix
      | j > borderD = getOut lookup imgSh ix
      | otherwise   = lookup ix

fragmentMap :: (Repa.Elt a, Num a, Repa.Elt b) => ((Repa.Array DIM2 a) -> b) ->
               Repa.Array DIM4 a -> Repa.Array DIM2 b
fragmentMap f array =
  array `Repa.deepSeqArray` Repa.force $
  Repa.traverse array newSh traverseFunc
  where
    newSh (Z :. x1 :. x2 :. _ :. _) =
      (Z :. x1 :. x2)
    traverseFunc _ ix =
      f $ Repa.slice array (ix :. All :. All)