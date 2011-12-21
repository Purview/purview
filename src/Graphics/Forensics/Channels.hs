module Graphics.Forensics.Channels
       ( -- * Channels
         Channels(..)
         -- ** File system
       , readChannels
       , writeChannels
         -- ** Formats
       , floatToByteChannels
       , byteToFloatChannels
         -- ** Helper functions
       , mapChannels
         -- ** Alpha utility functions
       , hasAlphaChannel
       , addAlphaChannel
       , removeAlphaChannel
       ) where

import Data.Array.Repa (Array, DIM3, (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import Data.Word

import Graphics.Forensics.Matrix

-- | A set of 'Channels' is a 3-dimensional matrix of numbers, where
-- the matrix's first two index coordinates specify the pixel, and
-- the third index coordinate specifies the color channel index.
data Channels n =
  -- | A channel matrix with 3 channels
  RGBChannels
  { channelMatrix :: Array DIM3 n
  } |
  -- | A channel matrix with 4 channels
  RGBAChannels
  { channelMatrix :: Array DIM3 n
  }

-- | Loads a channel matrix from a file. Very many image formats are
-- supported. The channels are guaranteed to be in RGBA format.
readChannels :: FilePath -> IO (Channels Word8)
readChannels = fmap RGBAChannels . Repa.runIL . Repa.readImage

-- | Saves the specified 'Channels' matrix to the specified path.
-- The image format is inferred from the file suffix.
writeChannels :: FilePath -> Channels Word8 -> IO ()
writeChannels =
  (. channelMatrix . addAlphaChannel 255) .
  fmap Repa.runIL . Repa.writeImage

-- | Converts a 'Float'-based channel matrix to a 'Word8'-based one.
floatToByteChannels :: Channels Float -> Channels Word8
floatToByteChannels = mapChannels floatToByte

-- | Converts a 'Word8'-based channel matrix to a 'Float'-based one.
byteToFloatChannels :: Channels Word8 -> Channels Float
byteToFloatChannels = mapChannels byteToFloat

-- | Performs an arbitrary operation on the channel matrix of some
-- 'Channels'.
mapChannels :: (Repa.Elt n1, Repa.Elt n2)
               => (n1 -> n2) -> Channels n1 -> Channels n2
mapChannels f (RGBAChannels matrix) = RGBAChannels . Repa.map f $ matrix
mapChannels f (RGBChannels matrix)  = RGBChannels  . Repa.map f $ matrix

-- | Removes an alpha channel from the specified 'Channels', or does
-- nothing if there is no alpha channel.
removeAlphaChannel :: (Repa.Elt n) => Channels n -> Channels n
removeAlphaChannel =
  RGBChannels . Repa.force . flip2 Repa.traverse decrChannel id . channelMatrix
  where
    {-# INLINE decrChannel #-}
    decrChannel (s :. _) = s :. 3

-- | Adds an alpha channel with the specified constant value, if the
-- channels don't already have an alpha channel.
addAlphaChannel :: (Repa.Elt n) => n -> Channels n -> Channels n
addAlphaChannel value =
  until hasAlphaChannel $
  RGBAChannels .
  Repa.force . flip2 Repa.traverse incrChannel setAlpha .
  channelMatrix
  where
    {-# INLINE incrChannel #-}
    incrChannel (ds :. 3) = ds :. 4
    incrChannel sh        = sh
    {-# INLINE setAlpha #-}
    setAlpha _    (_ :. 3) = value
    setAlpha look coord    = look coord

-- | Checks whether the 'Channels' contain an alpha channel
hasAlphaChannel :: (Repa.Elt n) => Channels n -> Bool
hasAlphaChannel RGBAChannels {} = True
hasAlphaChannel RGBChannels  {} = False

{-# INLINE flip2 #-}
flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c
