module Graphics.Forensics.Analyser.Demosaic where

import Prelude hiding (zipWith3)

import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Utilities (zipWith3)
import Data.Array.Repa (Array(..), DIM2, (:.)(..), Source(..), computeP)
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.Unboxed (U)

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = demosaicAnalyse
  , name = "demosaic"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

demosaic :: (Monad m) => ByteImage -> m (ByteImage)
demosaic img = do
  hpf <- highpassFilter . byteToFloatImage $ img
  return (floatToByteImage hpf)

{-# NOINLINE highpassFilter #-}
highpassFilter :: (Monad m) => FloatImage -> m (FloatImage)
highpassFilter img = do
  let conv = return . convolveS Clamp highpass
  r <- getR img >>= conv
  g <- getG img >>= conv
  b <- getB img >>= conv
  Repa.computeP $ zipWith3 fromRGBValues r g b

getR :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
getR i = computeP $ Repa.map (\(RGBA r _ _ _) -> r) i

getG :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
getG i = computeP $ Repa.map (\(RGBA _ g _ _) -> g) i

getB :: (Monad m) => FloatImage -> m (Array U DIM2 Float)
getB i = computeP $ Repa.map (\(RGBA _ _ b _) -> b) i

demosaicAnalyse :: ByteImage -> Analysis ()
demosaicAnalyse img = do
  result <- demosaic img
  reportInfo "Image demosaic analysis" $ reportImage result
