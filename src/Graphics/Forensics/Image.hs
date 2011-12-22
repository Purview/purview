-- | Allows images to be treated like color matrices
module Graphics.Forensics.Image
       ( -- * Image
         Image
       , ByteImage
       , FloatImage
         -- ** File system
       , readImage
       , writeImage
         -- ** Formats
       , floatToByteImage
       , byteToFloatImage
         -- ** Conversion
       , splitChannels
       , mergeChannels
       ) where

import Data.Array.Repa (Array, DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Word

import TypeLevel.NaturalNumber

import Graphics.Forensics.Array
import Graphics.Forensics.Channels
import Graphics.Forensics.Color

-- | An 'Image' is a 2-dimensional array of 'RGBA' colors
type Image n = Array DIM2 (RGBA n)

type ByteImage = Image Word8

type FloatImage = Image Float

-- | Loads an 'Image' array from a file. Very many image formats are
-- supported.
readImage :: FilePath -> IO (Image Word8)
readImage = fmap mergeChannels . readChannels

-- | Saves the specified 'Image' array to the specified path.
-- The image format is inferred from the file suffix.
writeImage :: FilePath -> Image Word8 -> IO ()
writeImage = (. splitChannels) . writeChannels

-- | Converts a 128-bit floating point color image to a 32-bit color image
floatToByteImage :: Image Float -> Image Word8
floatToByteImage = Repa.map $ mapColor floatToByte

-- | Converts a 32-bit color image to a 128-bit floating point color image
byteToFloatImage :: Image Word8 -> Image Float
byteToFloatImage = Repa.map $ mapColor byteToFloat

-- | Separates an image into a 3D array, where the first two
-- coordinates specify the pixel coordinate, and the third coordinate
-- specifies the channel index.
splitChannels :: (Repa.Elt n) => Image n -> Channels N4 n
splitChannels =
  makeChannels . flip2 Repa.traverse addChannel writeRGBColor
  where
    {-# INLINE addChannel #-}
    addChannel s = s :. 4
    {-# INLINE writeRGBColor #-}
    writeRGBColor look (coord :. c) = channel c . look $ coord
    {-# INLINE channel #-}
    channel 0 = channelRed
    channel 1 = channelGreen
    channel 2 = channelBlue
    channel 3 = channelAlpha
    channel _ = undefined

-- | Merges a 3D channel array into an SRGBA color image.
mergeChannels :: (Repa.Elt n) => Channels N4 n -> Image n
mergeChannels =
  flip2 Repa.traverse dropChannel readRGBColor . channelArray
  where
    {-# INLINE dropChannel #-}
    dropChannel (s :. _) = s
    {-# INLINE readRGBColor #-}
    readRGBColor look coord =
      let color i = look $ coord :. i
      in RGBA (color 0) (color 1) (color 2) (color 3)

{-# INLINE mapColor #-}
mapColor :: (a -> b) -> RGBA a -> RGBA b
mapColor f (RGBA r g b a) = RGBA (f r) (f g) (f b) (f a)

{-# INLINE flip2 #-}
flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c
