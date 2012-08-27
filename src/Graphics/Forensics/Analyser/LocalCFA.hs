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

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "Moritz Roth"
  , version = readVersion "1.0"
  }

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse img = task "Local CFA analysis" 4 $ do
  {- Extract the green channel and convolve with a highpass filter -}
  hpf <- highpassFilter . byteToFloatImage $ img
  step
  {- Split the image into overlapping fragments, and map the local  -}
  {- CFA detection function over them -}
  filtered <- computeUnboxedP $ fragmentMap localAnalysis (Z :. 32 :. 32) hpf
  step
  {- Return the resulting grayscale image (as RGBA) -}
  rgbaResult <- computeUnboxedP $ Repa.map fromGrayscaleFloat filtered
  step
  reportInfo "Output: local CFA peak size mapped image."
    $ reportImage (floatToByteImage rgbaResult)

-- | Extracts the green channel of the image and convolves it with a
  -- highpass filter
highpassFilter :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
highpassFilter !i = do
  greenChannel <- computeUnboxedP $ Repa.map ((* 255) . channelGreen) i
  computeP $ convolveS Clamp highpass greenChannel

-- | Returns the normalised local CFA peak size for an array fragment
localAnalysis :: Array D DIM2 Float -> Float
localAnalysis !a =
  getPeakValue ix . normalise ix $ magnitudes
  where
    !ix = len `div` 2
    !len = V.length diags
    magnitudes = dftMagnitude len diags
    diags = getDiagonalVariances a

-- | Computes the DFT of a vector and returns the magnitudes of the result
{-# INLINE dftMagnitude #-}
dftMagnitude :: Int -> Vector Float -> Vector Float
dftMagnitude len a =
  V.map floatMagnitude . Repa.toUnboxed $ dftc
  where
    dftc = dftS list
    list = Repa.fromUnboxed (Z :. len) . V.map realToComplex $ a

-- | Returns the variances of all diagonals in the given array as a vector
{-# INLINE getDiagonalVariances  #-}
getDiagonalVariances :: (Source r1 Float) =>
                       Array r1 DIM2 Float -> Vector Float
getDiagonalVariances !arr =
  getDiagonals $ w + h - 1
  where
    (Z :. w :. h) = extent arr
    {- Get the variance of all diagonals in the array -}
    {-# INLINE getDiagonals #-}
    getDiagonals :: Int -> Vector Float
    getDiagonals s = V.generate s $ variance . getDiagonalAt

    {- Get a single diagonal at position n in x direction -}
    {-# INLINE getDiagonalAt #-}
    getDiagonalAt :: Int -> Vector Float
    getDiagonalAt n =
      V.generate d getFromArray
      where
        x0 = min n (w - 1)
        y0 = max 0 (n - w + 1)
        xN = max 0 (w - h + y0)
        d = 1 + (x0 - xN)
        {-# INLINE getFromArray #-}
        getFromArray :: Int -> Float
        getFromArray a = arr `unsafeIndex` (Z :. x0 - a :. y0 + a)

{-As described in the paper, this computes the mean instead of real variance-}
{-# INLINE variance  #-}
variance :: Vector Float -> Float
variance !a =
  s / l
  where
    (s, l) = V.foldl' (\(su, le) n -> (su + n, le + 1)) (0, 0) a

{- Removes DC value and normalises by the median -}
{-# INLINE normalise #-}
normalise :: Int -> Vector Float -> Vector Float
normalise ix !list =
  V.map (/ median) (V.unsafeTail list)
  where
    median = sorted `V.unsafeIndex` ix
    !sorted = V.modify (\v -> V.select v ix) list

{- Gets the peak value from the DFT spectrum and scales it -}
{-# INLINE getPeakValue #-}
getPeakValue :: Int -> Vector Float -> Float
getPeakValue mid !list =
  peak / maxVal
  where
    peak = V.maximum $ V.unsafeSlice (mid - 1) 3 list
    maxVal = V.maximum list

{-# INLINE floatMagnitude #-}
floatMagnitude :: Complex -> Float
floatMagnitude (r, c) =
  sqrt (fr * fr + fc * fc)
  where
    fr = double2Float r
    fc = double2Float c
