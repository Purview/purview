module Graphics.Forensics.Analyser.Demosaic where

import Prelude hiding (zipWith3)
import Control.Monad

import Graphics.Forensics.Algorithms
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Utilities (zipWith3)
import Data.Array.Repa (DIM2, (:.)(..), computeP, computeUnboxedP)
import qualified Data.Array.Repa as Repa

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
demosaic = liftM floatToByteImage . highpassFilter . byteToFloatImage

{-# NOINLINE highpassFilter #-}
highpassFilter :: (Monad m) => FloatImage -> m (FloatImage)
highpassFilter !img = do
  let conv = return . convolveS Clamp highpass
  let rmap = computeUnboxedP . flip Repa.map img
  r <- conv =<< rmap channelRed
  g <- conv =<< rmap channelGreen
  b <- conv =<< rmap channelBlue
  computeP $ zipWith3 fromRGBValues r g b

demosaicAnalyse :: ByteImage -> Analysis ()
demosaicAnalyse img = do
  result <- demosaic img
  reportInfo "Image demosaic analysis" $ reportImage result
