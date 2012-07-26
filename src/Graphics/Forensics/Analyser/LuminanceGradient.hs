module Graphics.Forensics.Analyser.LuminanceGradient where
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Array(..), Z(..), DIM2, (:.)(..), Source(..), D)
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.Unboxed (U)

sobelX :: Array U DIM2 Float
--sobelX = [stencil2| -1 0 1
--                    -2 0 2
--                    -1 0 1 |]
sobelX = Repa.fromListUnboxed (Z :. 3 :. 3) [-1, 0, 1, -2, 0, 2, -1, 0, 1]

sobelY :: Array U DIM2 Float
--sobelY = [stencil2| -1 -2 -1
--                     0  0  0
--                     1  2  1 |]
sobelY = Repa.fromListUnboxed (Z :. 3 :. 3) [-1, -2, -1, 0, 0, 0, 1, 2, 1]

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = lgAnalyse
  , name = "lg"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "1.0"
  }

luminanceGradient :: (Monad m) => ByteImage -> m (ByteImage)
luminanceGradient img = do
  grayscale <- Repa.computeP $ grayscaleImage . byteToFloatImage $ img
  (gX, gY) <- gradients grayscale
  lgImage <- Repa.computeP $ Repa.zipWith gradientColour gX gY
  return (floatToByteImage lgImage)

gradients :: (Monad m) => Array U DIM2 Float ->
             m ((Array U DIM2 Float, Array U DIM2 Float))
gradients i = do
  gX <- edgeX i
  gY <- edgeY i
  return (gX, gY)

{-# NOINLINE grayscaleImage #-}
grayscaleImage :: FloatImage -> Array D DIM2 Float
grayscaleImage =
  Repa.map rgbaToGrayscale

{-# INLINE rgbaToGrayscale #-}
rgbaToGrayscale :: RGBA Float -> Float
rgbaToGrayscale (RGBA r g b _) =
  0.2126 * r + 0.7152 * g + 0.0722 * b

{-# NOINLINE edgeX #-}
edgeX :: (Monad m) => Array U DIM2 Float -> m (Array U DIM2 Float)
edgeX =
  convolve Clamp sobelX

{-# NOINLINE edgeY #-}
edgeY :: (Monad m) => Array U DIM2 Float -> m (Array U DIM2 Float)
edgeY =
  convolve Clamp sobelY

{-# INLINE gradientColour #-}
gradientColour ::  Float -> Float -> RGBA Float
gradientColour gx gy =
  RGBA r g b 1.0
  where
    angle = atan2 gy gx
    r     = getR angle
    g     = getG angle
    b     = magnitude gx gy
    {-# INLINE magnitude #-}
    magnitude x y = sqrt (x * x + y * y)
    {-# INLINE getR #-}
    getR = (0.5 +) . (/ 2) . negate . sin
    {-# INLINE getG #-}
    getG = (0.5 +) . (/ 2) . negate . cos

lgAnalyse :: ByteImage -> Analysis ()
lgAnalyse img = do
  result <- luminanceGradient img
  reportInfo "Luminance gradient mapped image" $ reportImage result
