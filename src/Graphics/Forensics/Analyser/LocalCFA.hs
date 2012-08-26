module Graphics.Forensics.Analyser.LocalCFA where

import GHC.Float

import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report

import Data.Array.Repa (Source(..), Z(..),  DIM2, (:.)(..), Array(..),
                        U, D, computeP, computeUnboxedP)
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Algorithms.Complex

import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

import Control.Monad.ST

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

localcfa :: (Monad m) => ByteImage -> m ByteImage
localcfa !img = do
  hpf <- highpassFilter . byteToFloatImage $ img
  let fragments = fragmentMap localAnalysis (Z :. 32 :. 32) hpf
  filtered <- computeUnboxedP fragments
  rgbaResult <- computeUnboxedP $ Repa.map fromGrayscaleFloat filtered
  return (floatToByteImage rgbaResult)

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

highpassFilter :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
highpassFilter !i = do
  greenChannel <- computeUnboxedP $ Repa.map ((* 255) . channelGreen) i
  computeP $ convolveS Clamp highpass greenChannel

localAnalysis :: Array D DIM2 Float -> Float
localAnalysis !a =
  getPeakValue . normalise $ magnitudes
  where
    diags = getDiagonalVariances a
    magnitudes = dftMagnitude diags

{-# INLINE floatMagnitude #-}
floatMagnitude :: Complex -> Float
floatMagnitude (r, c) =
  sqrt (fr * fr + fc * fc)
  where
    fr = double2Float r
    fc = double2Float c

{-# INLINE dftMagnitude #-}
dftMagnitude :: Vector Float -> Vector Float
dftMagnitude a =
  V.map floatMagnitude . Repa.toUnboxed $ dftc
  where
    !list = Repa.fromUnboxed (Z :. (V.length a)) . V.map realToComplex $ a
    dftc = dftS list

-- | Returns the variances of all diagonals in the given array as a list
{-# INLINE getDiagonalVariances  #-}
getDiagonalVariances :: (Source r1 Float) =>
                       Array r1 DIM2 Float -> Vector Float
getDiagonalVariances !arr =
   V.fromList . map variance $ getDiagonals (w + h - 1)
  where
    (Z :. w :. h) = extent arr
    {-# INLINE getDiagonals #-}
    getDiagonals :: Int -> [Vector Float]
    getDiagonals 0 = []
    getDiagonals s = (getDiagonalAt s 0) : (getDiagonals $ s - 1)

    {-# INLINE getDiagonalAt #-}
    getDiagonalAt :: Int -> Int -> Vector Float
    getDiagonalAt x n
      | n >= h || x - n < 0 = V.empty
      | x - n > w = getDiagonalAt x $ n + 1
      | otherwise = arr `unsafeIndex` (Z :. x-n :. n) `V.cons`
                    getDiagonalAt x (n + 1)

{-As described in the paper, this computes the mean instead of real variance-}
{-# INLINE variance  #-}
variance :: Vector Float -> Float
variance !a =
  let (s, l) = V.foldl' (\(su, le) n -> (su + n, le + 1)) (0, 0) a in
  s / l

{-# INLINE normalise #-}
normalise :: Vector Float -> Vector Float
normalise !list =
  V.map (/ median) (V.unsafeTail list)
  where
    median = sorted `V.unsafeIndex` ix
    ix = V.length list `div` 2
    !sorted = runST $ do
      v <- V.thaw list
      V.select v ix
      V.unsafeFreeze v

{-# INLINE getPeakValue #-}
getPeakValue :: Vector Float -> Float
getPeakValue !list =
  peak / maxVal
  where
    peak = V.maximum $ V.unsafeSlice (mid - 1) 3 list
    mid  = V.length list `div` 2
    maxVal = V.maximum list

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse img = do
  result <- localcfa img
  reportInfo "Local CFA peak size mapped image" $ reportImage result
