module Graphics.Forensics.Analyser.LuminanceGradient where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Stencil

sobelX :: Stencil DIM2 Float
sobelX = [stencil2| -1 0 1
                    -2 0 2
                    -1 0 1 |]

sobelY :: Stencil DIM2 Float
sobelY = [stencil2| -1 -2 -1
                     0  0  0
                     1  2  1 |]

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = lgAnalyse
  , name = "lg"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "1.0"
  }

luminanceGradient :: ByteImage -> ByteImage
luminanceGradient img =
  lgImage `Repa.deepSeqArray` floatToByteImage $ lgImage
  where
    grayscale = grayscaleImage . byteToFloatImage $ img
    (gX, gY)  = grayscale `Repa.deepSeqArray` gradients $ grayscale
    lgImage   = Repa.force2 $ Repa.zipWith gradientColour gX gY

gradients :: Repa.Array DIM2 Float ->
             (Repa.Array DIM2 Float, Repa.Array DIM2 Float)
gradients =
  Repa.withManifest $ \i ->
  let (gX, gY) = (edgeX i, edgeY i) in Repa.deepSeqArrays [gX, gY] (gX, gY)

{-# NOINLINE grayscaleImage #-}
grayscaleImage :: FloatImage -> Repa.Array DIM2 Float
grayscaleImage =
  Repa.withManifest $ \i ->
  Repa.force2 $ Repa.map rgbaToGrayscale i

{-# INLINE rgbaToGrayscale #-}
rgbaToGrayscale :: RGBA Float -> Float
rgbaToGrayscale (RGBA r g b _) =
  0.2126 * r + 0.7152 * g + 0.0722 * b

{-# NOINLINE edgeX #-}
edgeX :: Repa.Array DIM2 Float -> Repa.Array DIM2 Float
edgeX img =
  Repa.deepSeqArray img $ Repa.force2 $ mapStencil2 BoundClamp sobelX img

{-# NOINLINE edgeY #-}
edgeY :: Repa.Array DIM2 Float -> Repa.Array DIM2 Float
edgeY img =
  Repa.deepSeqArray img $ Repa.force2 $ mapStencil2 BoundClamp sobelY img

{-# INLINE gradientColour #-}
gradientColour ::  Float -> Float -> RGBA Float
gradientColour gx gy =
  RGBA r g b 1.0
  where
    angle = atan2 gy gx
    r     = getR angle
    g     = getG angle
    b     = modulus gx gy
    {-# INLINE modulus #-}
    modulus x y = sqrt (x * x + y * y)
    {-# INLINE getR #-}
    getR = (0.5 +) . (/ 2) . negate . sin
    {-# INLINE getG #-}
    getG = (0.5 +) . (/ 2) . negate . cos

lgAnalyse :: ByteImage -> Analysis ()
lgAnalyse = reportInfo "Luminance gradient mapped image" .
            reportImage .
            luminanceGradient
