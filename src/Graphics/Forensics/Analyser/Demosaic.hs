module Graphics.Forensics.Analyser.Demosaic where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Color
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Data.Array.Repa (DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Stencil

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

demosaic :: ByteImage -> ByteImage
demosaic img =
  dmImage `Repa.deepSeqArray` floatToByteImage $ dmImage
  where
    dmImage  = normalise $ Repa.force2 filtered
    filtered = highpassFilter . byteToFloatImage $ img

normalise :: FloatImage -> FloatImage
normalise img =
  Repa.force $ Repa.map (liftRGBA (flip (/)) maxValue) img
  where
    maxValue = Repa.foldAll (liftRGBA max) (RGBA 0 0 0 0) img

liftRGBA :: (a -> b -> c) -> RGBA a -> RGBA b -> RGBA c
liftRGBA f (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) =
  RGBA (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

{-# NOINLINE highpassFilter #-}
highpassFilter :: FloatImage -> FloatImage
highpassFilter img =
  Repa.deepSeqArray merged merged
  where
    merged = r `Repa.deepSeqArray` g `Repa.deepSeqArray`
             b `Repa.deepSeqArray` (mergeArrays3 r g b) $ returnRGBA
    r      = hp . imap getR $ img
    g      = hp . imap getG $ img
    b      = hp . imap getB $ img
    hp arr = Repa.force2 $ mapStencil2 BoundClamp highpass arr

getR :: RGBA Float -> Float
getR (RGBA r _ _ _) = r

getG :: RGBA Float -> Float
getG (RGBA _ g _ _) = g

getB :: RGBA Float -> Float
getB (RGBA _ _ b _) = b

imap :: (Repa.Elt a, Repa.Elt b) =>
        (a -> b) -> Repa.Array DIM2 a -> Repa.Array DIM2 b
imap f =
  Repa.withManifest $ \arr -> Repa.force2 $ Repa.map f arr

mergeArrays3 :: Repa.Array DIM2 Float -> Repa.Array DIM2 Float ->
                Repa.Array DIM2 Float ->
                (Float -> Float -> Float -> RGBA Float) -> FloatImage
mergeArrays3 a1 a2 a3 f =
  Repa.force2 $ Repa.traverse3 a1 a2 a3 (\sh _ _ -> sh) $
  (\get1 get2 get3 ix -> f (get1 ix) (get2 ix) (get3 ix))

returnRGBA :: Float -> Float -> Float -> RGBA Float
returnRGBA r g b =
  (RGBA r g b 1.0)

demosaicAnalyse :: ByteImage -> Analysis ()
demosaicAnalyse = reportInfo "Image demosaic analysis" .
            reportImage .
            demosaic
