module Graphics.Forensics.Analyser.LocalCFA where

import Data.List
import GHC.Float
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Source(..), Z(..),  DIM2, (:.)(..),
                        U, D, computeP, computeUnboxedP, (!))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import Data.Array.Repa.Algorithms.Complex

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

localcfa :: (Monad m) => ByteImage -> m ByteImage
localcfa img = do
  hpf <- highpassFilter . byteToFloatImage $ img
  let fragments = fragmentMap localAnalysis (Z :. 32 :. 32) hpf
  filtered <- computeUnboxedP $ fragments
  rgbaResult <- computeUnboxedP $ Repa.map fromGrayscaleFloat filtered
  return (floatToByteImage rgbaResult)

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

highpassFilter :: (Monad m) => FloatImage -> m (Repa.Array U DIM2 Float)
highpassFilter i = do
  let conv = return . convolveS Clamp highpass
  computeP =<< conv =<< getG i

localAnalysis :: Repa.Array D DIM2 Float -> Float
localAnalysis a =
  getPeakValue . normalise $ magnitudes
  where
    diags = getDiagonalVariances a
    magnitudes = dftMagnitude diags

getG :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
getG i = computeP $ Repa.map (\(RGBA _ g _ _) -> g * 255) i

{-# INLINE floatMagnitude #-}
floatMagnitude :: Complex -> Float
floatMagnitude (r, c) =
  sqrt (fr * fr + fc * fc)
  where
    fr = double2Float r
    fc = double2Float c

{-# INLINE dftMagnitude #-}
dftMagnitude :: [Float] -> [Float]
dftMagnitude a =
  map floatMagnitude . Repa.toList $ dftc
  where
    list = Repa.fromList (Z :. (length a)) . map realToComplex $ a
    dftc = dftS list

-- | Returns the variances of all diagonals in the given array as a list
getDiagonalVariances :: (Source r1 Float) =>
                       Repa.Array r1 DIM2 Float -> [Float]
getDiagonalVariances arr =
  map variance $ getDiagonals (w + h - 1)
  where
    (Z :. w :. h) = extent arr
    getDiagonals :: Int -> [[Float]]
    getDiagonals 0 = []
    getDiagonals s = (getDiagonalAt s 0) : (getDiagonals $ s - 1)

    getDiagonalAt :: Int -> Int -> [Float]
    getDiagonalAt x n
      | n >= h || x - n < 0 = []
      | x - n > w = getDiagonalAt x $ n + 1
      | otherwise = arr ! (Z :. x - n :. n) : getDiagonalAt x (n + 1)


variance :: [Float] -> Float
variance a =
  let (s, l) = foldl' (\(su, le) n -> (su + n, le + 1)) (0, 0) a in
  s / l

normalise :: [Float] -> [Float]
normalise list =
  map (/ median) (tail list)
  where
    median = sorted !! ((length list) `div` 2)
    sorted = sort list

getPeakValue :: [Float] -> Float
getPeakValue list =
  peak / maxVal
  where
    peak = maximum . take 3 . drop (mid - 1) $ list

    mid    = length list `div` 2
    maxVal = maximum list

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse img = do
  result <- localcfa img
  reportInfo "Local CFA peak size mapped image" $ reportImage result
