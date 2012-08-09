module Graphics.Forensics.Analyser.LocalCFA where

import Data.List
import GHC.Float
import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (Source(..), Z(..),  DIM2, (:.)(..),
                        U, D, computeP, computeUnboxedS, computeUnboxedP)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import Data.Array.Repa.Algorithms.Complex
import System.IO.Unsafe

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
  let filtered = computeUnboxedS $ fragments
  rgbaResult <- computeUnboxedP $ Repa.map fromGrayscaleFloat filtered
  return (floatToByteImage rgbaResult)

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

highpassFilter :: (Monad m) => FloatImage -> m (Repa.Array U DIM2 Float)
highpassFilter i = do
  let conv = return . convolveS Clamp highpass
  g <- getG i >>= conv
  computeP g

localAnalysis :: Repa.Array D DIM2 Float -> Float
localAnalysis a = unsafePerformIO $ do
  let diags = map variance $ getDiagonals 32 $ Repa.toList a
  magnitudes <- dftMagnitude diags
  return $ getPeakValue . normalise $ magnitudes

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
dftMagnitude :: (Monad m) => [Float] -> m ([Float])
dftMagnitude a = do
  let list = Repa.fromList (Z :. (length a)) . map realToComplex $ a
  dftc <- dftP list
  return $ map floatMagnitude . Repa.toList $ dftc

getDiagonals :: Int -> [Float] -> [[Float]]
getDiagonals fragmentSize array = do
  size <- [0..fragmentSize]
  return $
    take (fragmentSize - 2 * size) .
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
    peak   = maximum . take 3 . drop (mid - 1) $ list
    mid    = length list `div` 2
    maxVal = maximum list

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse img = do
  result <- localcfa img
  reportInfo "Local CFA peak size mapped image" $ reportImage result
