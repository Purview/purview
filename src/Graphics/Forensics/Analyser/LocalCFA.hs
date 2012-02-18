module Graphics.Forensics.Analyser.LocalCFA where

import Control.Monad

import Graphics.Forensics.Analyser
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Shape

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

localcfa :: ByteImage -> ByteImage
localcfa = floatToByteImage .
           fragmentMap $
           getDFT .
           map variance .
           getDiagonals .
           Repa.toList
           fragmentize Clamp (Z :. 32 :. 32).
           applyHighpassFilter .
           extractGreen .
           byteToFloatImage

highpassFilter :: Repa.Array DIM2 Float
highpassFilter =
  Repa.fromList (Z :. 3 : 3) [0, 1, 0, 1, -4, 1, 0, 1, 0]

applyHighpassFilter :: Repa.Array DIM2 Float -> Repa.Array DIM2 Float
applyHighpassFilter = convolve Clamp highpassFilter

{- INLINE getG -}
getG :: RGBA Float -> Float
getG (RGBA _ g _ _) = g

extractGreen :: FloatImage -> Repa.Array DIM2 Float
extractGreen = Repa.force . Repa.map getG

getDFT :: [Float] -> [Float]
getDFT = dft1d Forward . map realToComplex

getDiagonals :: Float -> [Float] -> [[Float]]
getDiagonals fragmentSize array = do
  size <- [0..fragmentSize]
  return getDiagonalValues fragmentSize (drop size array)

getDiagonalValues :: Float -> [Float] -> [Float]
--PRE: Array is quadratic
getDiagonalValues fragmentSize array =
  every (fragmentSize + 1) array
  where
    every :: Int -> [a] -> [a]
    every _ []       = []
    every n (x : xs) = x : every n (drop n xs)

variance :: [Float] -> Float
variance a =
  (fromIntegral $ sum a) / (fromIntegral $ length a)

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse = undefined