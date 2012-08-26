module Graphics.Forensics.Analyser.LuminanceGradient where
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Array(..), DIM2, (:.)(..), Source(..), computeP)
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.Unboxed (U)

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

luminanceGradient :: (Monad m) => ByteImage -> m (ByteImage)
luminanceGradient !img = do
  grayscale <- computeP $ toGrayscaleImage 255 img
  let gX = edgeX grayscale
  let gY = edgeY grayscale
  lgImage <- computeP $ Repa.zipWith gradientColour gX gY
  return $ floatToByteImage lgImage

{-# INLINE edgeX #-}
edgeX :: Array U DIM2 Float -> Array PC5 DIM2 Float
edgeX !a = convolveS Clamp sobelX a

{-# INLINE edgeY #-}
edgeY :: Array U DIM2 Float -> Array PC5 DIM2 Float
edgeY !a = convolveS Clamp sobelY a

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
