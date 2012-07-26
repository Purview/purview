-- | Provides algorithms to manipulate matrices
module Graphics.Forensics.Algorithms
       ( -- * Convolution
         OutOfRangeMode(..),
         convolve,
         -- * Discrete Fourier Transform
         Mode(..),
         realToComplex,
         complexRound,
         DFT.fft1dP,
         DFT.fft2dP,
         DFT.fft3dP,
         -- * Fragmentize
         fragmentize,
         fragmentMap
       ) where
import Prelude hiding (lookup)
import qualified Data.Array.Repa.Algorithms.Convolve as Repa
import qualified Data.Array.Repa.Algorithms.FFT as DFT
import Data.Array.Repa.Algorithms.FFT (Mode(..))
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa (Array(..), Z(..), DIM2, DIM4, All(..), (:.)(..),
                        extent, Source(..), D)
import Data.Array.Repa.Repr.Unboxed (U, Unbox)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa

-- | Provides the different edge case handling methods for convolution
data OutOfRangeMode a = Clamp | Value a | Function (Repa.GetOut a)

-- | Convolves a matrix using the specified kernel and edge handling method
convolve :: (Unbox n, Num n, Monad m) =>
            OutOfRangeMode n -> Array U DIM2 n ->
            Array U DIM2 n -> m (Array U DIM2 n)
convolve (Clamp)      = Repa.convolveOutP Repa.outClamp
convolve (Value a)    = Repa.convolveOutP $ Repa.outAs a
convolve (Function f) = Repa.convolveOutP f

realToComplex :: (Real a) => a -> Complex
realToComplex x =
  (realToFrac x, 0.0)

complexRound :: Complex -> Complex
complexRound (a, b) =
  (fromInteger $ round a, fromInteger $ round b)

fragmentize :: (Repa.Elt a, Num a, Repa.Source rep a) =>
               OutOfRangeMode a -> DIM2 -> Array rep DIM2 a
               -> Array D DIM4 a
fragmentize (Clamp) kSh image =
  let iSh = extent image in
  Repa.traverse image (shapeConv kSh) (gen Repa.outClamp iSh kSh)
fragmentize (Value x) kSh image =
  let iSh = extent image in
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

fragmentMap :: (Repa.Elt a, Num a, Repa.Elt b, Source r1 a) =>
               ((Repa.Array D DIM2 a) -> b) ->
               Repa.Array r1 DIM4 a -> Repa.Array D DIM2 b
fragmentMap f array =
  Repa.traverse array newSh traverseFunc
  where
    newSh (Z :. x1 :. x2 :. _ :. _) =
      (Z :. x1 :. x2)
    traverseFunc _ ix =
      f $ Repa.slice array (ix :. All :. All)
