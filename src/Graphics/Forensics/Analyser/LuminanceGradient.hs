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

grayscaleImage :: ByteImage -> Repa.Array DIM2 Float
grayscaleImage a = a `Repa.deepSeqArray` Repa.map rgbaToGrayscale

rgbaToGrayscale :: RGBA Float -> Float
rgbaToGrayscale (RGBA r g b _) =
  0.2126 * r + 0.7152 * g + 0.0722 * b

gradient ::  Repa.Array DIM2 Float -> RGBA Float
gradient array = array `Repa.deepSeqArray` gradient' $ array
  where
    gradient' array =
      RGBA r g b 1.0
      where
        lx    = sum $ zipWith (*) (Repa.toList array) sobelX
        ly    = sum $ zipWith (*) (Repa.toList array) sobelY
        angle = atan2 ly lx
        r     = (- sin angle) / 2.0 + 0.5
        g     = (- cos angle) / 2.0 + 0.5
        b     = sqrt (lx * lx + ly * ly)

lgAnalyse :: ByteImage -> Analysis ()
lgAnalyse = reportInfo "Luminance gradient mapped image" .
            reportImage .
            luminanceGradient
