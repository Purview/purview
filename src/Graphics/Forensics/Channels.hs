-- | A representation of image data based on 3-dimensional 'Array's
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
       , mapChannelsArray
         -- ** Alpha utility functions
       , hasAlphaChannel
       , addAlphaChannel
       , removeAlphaChannel
       ) where

import Data.Array.Repa (Array, DIM3, (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import Data.Word

import Graphics.Forensics.Array

-- | A set of 'Channels' is a 3-dimensional array of numbers, where
-- the array's first two index coordinates specify the pixel, and
-- the third index coordinate specifies the color channel index.
data Channels n =
  -- | A channel array with 3 channels
  RGBChannels
  { channelArray :: Array DIM3 n
  } |
  -- | A channel array with 4 channels
  RGBAChannels
  { channelArray :: Array DIM3 n
  } deriving (Show, Eq)

-- | Loads a channel array from a file. Very many image formats are
-- supported. The channels are guaranteed to be in RGBA format.
readChannels :: FilePath -> IO (Channels Word8)
readChannels = fmap RGBAChannels . Repa.runIL . Repa.readImage

-- | Saves the specified 'Channels' array to the specified path.
-- The image format is inferred from the file suffix.
writeChannels :: FilePath -> Channels Word8 -> IO ()
writeChannels =
  (. channelArray . addAlphaChannel 255) .
  fmap Repa.runIL . Repa.writeImage

-- | Converts a 'Float'-based channel array to a 'Word8'-based one.
floatToByteChannels :: Channels Float -> Channels Word8
floatToByteChannels = mapChannels floatToByte

-- | Converts a 'Word8'-based channel array to a 'Float'-based one.
byteToFloatChannels :: Channels Word8 -> Channels Float
byteToFloatChannels = mapChannels byteToFloat

-- | Performs an arbitrary operation on the channel array of some
-- 'Channels'.
mapChannels :: (Repa.Elt n1, Repa.Elt n2)
               => (n1 -> n2) -> Channels n1 -> Channels n2
mapChannels = mapChannelsArray . Repa.map

mapChannelsArray :: (Repa.Elt n1, Repa.Elt n2)
                     => (Array DIM3 n1 -> Array DIM3 n2)
                     -> Channels n1 -> Channels n2
mapChannelsArray f (RGBAChannels array) = RGBAChannels . f $ array
mapChannelsArray f (RGBChannels array)  = RGBChannels  . f $ array

-- | Removes an alpha channel from the specified 'Channels', or does
-- nothing if there is no alpha channel.
removeAlphaChannel :: (Repa.Elt n) => Channels n -> Channels n
removeAlphaChannel =
  RGBChannels . Repa.force . flip2 Repa.traverse decrChannel id . channelArray
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
  channelArray
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
