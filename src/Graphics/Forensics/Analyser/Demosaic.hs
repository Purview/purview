module Graphics.Forensics.Analyser.Demosaic where

import Prelude hiding (zipWith3)

import Graphics.Forensics.Algorithms.Convolve
import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Utilities (zipWith3)
import Data.Array.Repa (DIM2, (:.)(..), computeP, computeUnboxedP)
import qualified Data.Array.Repa as Repa

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = demosaicAnalyse
  , name = "demosaic"
  , author = "Moritz Roth"
  , version = readVersion "1.0"
  }

highpass :: Stencil DIM2 Float
highpass = [stencil2| 0  1  0
                      1 -4  1
                      0  1  0 |]

demosaicAnalyse :: ByteImage -> Analysis ()
demosaicAnalyse !img = task "Demosaic analysis" 4 $ do
  let fImg = byteToFloatImage img
  {-Convole the float image with a highpass filter-}
  let conv = return . convolveS Clamp highpass
  let rmap = computeUnboxedP . flip Repa.map fImg
  r <- conv =<< rmap channelRed
  step
  g <- conv =<< rmap channelGreen
  step
  b <- conv =<< rmap channelBlue
  step
  {-Merge the convolved channels into the result image-}
  result <- computeP $ zipWith3 fromRGBValues r g b
  reportInfo "Output: image demosaic analysis." $
    reportImage (floatToByteImage result)
