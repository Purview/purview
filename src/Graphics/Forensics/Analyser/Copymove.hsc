module Graphics.Forensics.Analyser.Copymove where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report

import Data.Array.Repa (Array(..), computeP, DIM2, extent, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.ForeignPtr (F)
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import System.IO.Unsafe

#include "copymove.h"

-- The actual analysis function from copymove.c
-- THIS DOES NOT WORK (YET). USE IT AND IT WILL COREDUMP
foreign import ccall unsafe "runAnalysis"
  c_runAnalysis :: Ptr Float -> CInt -> CInt -> CUChar ->
                   CFloat -> CUChar -> CInt -> IO(Ptr Float)
  -- input -> width -> height -> block size ->
  -- quality -> precompute size -> threshold -> output

-- Frees the result array
foreign import ccall "&" c_free :: FunPtr (Ptr a -> IO ())

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = copymoveAnalyse
  , name = "copymove"
  , author = "Moritz Roth"
  , version = readVersion "0.1"
  }

-- Quality factor to scale the DCT quantization matrix
quality :: CFloat
quality = 75

-- DCT block size
blockSize :: CUChar
blockSize = 16

pdctSize :: CUChar
pdctSize = 2

threshold :: CInt
threshold = 50

copymoveAnalyse :: ByteImage -> Analysis ()
copymoveAnalyse !img = task "Copy-move analysis" 1 $ do
  grayscaleImage <- grayscale img
  let sh@(Z :. w :. h) = extent grayscaleImage
      ptr = Repa.toForeignPtr grayscaleImage
      result = runAnalysis ptr (fromIntegral w) (fromIntegral h) sh
  cresult :: FloatImage <- computeP $ Repa.map fromGrayscaleFloat result
  reportInfo "Output: copy-move analysis." $
    reportImage (floatToByteImage cresult)

runAnalysis :: ForeignPtr Float -> CInt -> CInt -> DIM2 -> Array F DIM2 Float
runAnalysis fp w h rsh = unsafePerformIO $ do
  result <- withForeignPtr fp $ \ptr ->
    c_runAnalysis ptr w h blockSize quality pdctSize threshold
  newptr <- newForeignPtr c_free result
  return $ Repa.fromForeignPtr rsh newptr

grayscale :: (Monad m) => ByteImage -> m (Array F DIM2 Float)
grayscale !img = computeP $ toGrayscaleImage 255 img