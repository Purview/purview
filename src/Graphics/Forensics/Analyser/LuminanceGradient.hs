module Graphics.Forensics.Analyser.LuminanceGradient where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Z(..), DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa

sobelX :: [Float]
sobelX = [-1, 0, 1, -2, 0, 2, -1, 0, 1]

sobelY :: [Float]
sobelY = [1, 2, 1, 0, 0, 0, -1, -2, -1]

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = lgAnalyse
  , name = "lg"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

luminanceGradient :: ByteImage -> ByteImage
luminanceGradient = floatToByteImage .
                    fragmentMap gradient .
                    fragmentize Clamp (Z :. 3 :. 3) .
                    grayscaleImage .
                    byteToFloatImage

fragmentizeImage

grayscaleImage :: FloatImage -> Repa.Array DIM2 Float
grayscaleImage = Repa.force . Repa.map rgbaToGrayscale

{- INLINE rgbaToGrayscale -}
rgbaToGrayscale :: RGBA Float -> Float
rgbaToGrayscale (RGBA r g b _) =
  0.2126 * r + 0.7152 * g + 0.0722 * b

gradient ::  Repa.Array DIM2 Float -> RGBA Float
gradient array =
  RGBA r g b 1.0
  where
    lx    = convolve (Repa.toList array) sobelX
    ly    = convolve (Repa.toList array) sobelY
    angle = atan2 ly lx
    r     = getR angle
    g     = getG angle
    b     = modulus lx ly
    {- INLINE modulus -}
    modulus a b = sqrt (a * a + b * b)
    {- INLINE convolve -}
    convolve = sum . zipWith (*)
    {- INLINE getR -}
    getR  = (0.5 +) . (/ 2) . negate . sin
    {- INLINE getG -}
    getG  = (0.5 +) . (/ 2) . negate . cos

lgAnalyse :: ByteImage -> Analysis ()
lgAnalyse = reportInfo "Luminance gradient mapped image" .
            reportImage .
            luminanceGradient
