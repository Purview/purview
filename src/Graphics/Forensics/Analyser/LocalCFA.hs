module Graphics.Forensics.Analyser.LocalCFA where

import Data.List
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Z(..),  DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Algorithms.Complex
import GHC.Float

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

localcfa :: ByteImage -> ByteImage
localcfa =
  floatToByteImage .
  Repa.map returnGrayscale .
  fragmentMap (
    getPeakValue .
    normalise .
    sort .
    dftMagnitude .
    map variance .
    getDiagonals 32 .
    Repa.toList) .
  fragmentize Clamp (Z :. 32 :. 32).
  applyHighpassFilter .
  extractGreen .
  byteToFloatImage

highpassFilter :: Repa.Array DIM2 Float
highpassFilter =
  Repa.fromList (Z :. 3 :. 3) [0, 1, 0, 1, -4, 1, 0, 1, 0]

applyHighpassFilter :: Repa.Array DIM2 Float -> Repa.Array DIM2 Float
applyHighpassFilter =
  convolve Clamp highpassFilter

{- INLINE getG -}
getG :: RGBA Float -> Float
getG (RGBA _ g _ _) =
  g

{- INLINE returnGrayscale -}
returnGrayscale :: Float -> RGBA Float
returnGrayscale a =
  RGBA a a a 1.0

extractGreen :: FloatImage -> Repa.Array DIM2 Float
extractGreen =
  Repa.force . Repa.map getG

{- INLINE floatMagnitude -}
floatMagnitude :: Complex -> Float
floatMagnitude (r, c) =
  sqrt (fr * fr + fc * fc)
  where
    fr = double2Float r
    fc = double2Float c

dftMagnitude :: [Float] -> [Float]
dftMagnitude a =
  map floatMagnitude .
  Repa.toList .
  dft1d Forward .
  Repa.fromList (Z :. (length a) ) .
  map realToComplex $ a

getDiagonals :: Int -> [Float] -> [[Float]]
getDiagonals fragmentSize array = do
  size <- [0..fragmentSize]
  return $
    take (fragmentSize - size) .
    getDiagonalValues fragmentSize $
    (drop size array)

--PRE: Array is quadratic
getDiagonalValues :: Int -> [Float] -> [Float]
getDiagonalValues fragmentSize array =
  every (fragmentSize + 1) array
  where
    every :: Int -> [a] -> [a]
    every _ []       = []
    every n (x : xs) = x : every n (drop n xs)

variance :: [Float] -> Float
variance a =
  (sum a) / (fromIntegral $ length a)

normalise :: [Float] -> [Float]
normalise list =
  map (/ median) (tail list)
  where
    median = list !! ((length list) `div` 2)

getPeakValue :: [Float] -> Float
getPeakValue list =
  peak / maxVal
  where
    peak    = maximum . take 3 . drop (mid - 1) $ list
    mid     = length list `div` 2
    maxVal  = maximum list

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse =
  reportInfo "Local CFA peak size mapped image" .
  reportImage .
  localcfa