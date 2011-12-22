-- | A representation of image data based on 3-dimensional 'Array's
module Graphics.Forensics.Channels
       ( -- * Channels
         Channels
       , channelArray
       , makeChannels
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

import Data.Array.Repa (Array(..), DIM3, (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import Data.Word

import TypeLevel.NaturalNumber

import Graphics.Forensics.Array

-- | A set of 'Channels' is a 3-dimensional array of numbers, where
-- the array's first two index coordinates specify the pixel, and
-- the third index coordinate specifies the color channel index.
data Channels r n =
  Channels
  { -- | The array storing the channel matrix.
    channelArray :: Array DIM3 n
  }
  deriving (Show, Eq)

-- | Safely constructs 'Channels' from an array by checking that the
-- correct number of channels are present.
makeChannels :: forall n r . (Repa.Elt n, NaturalNumber r)
                => Array DIM3 n -> Channels r n
makeChannels arr @ (Array (_ :. nr) _) =
  if nr == naturalNumberAsInt (undefined :: r)
  then Channels arr
  else error "Array extent doesn't match requested channel count"

-- | Loads a channel array from a file. Very many image formats are
-- supported. The channels are guaranteed to be in RGBA format.
readChannels :: FilePath -> IO (Channels N4 Word8)
readChannels = fmap makeChannels . Repa.runIL . Repa.readImage

-- | Saves the specified 'Channels' array to the specified path.
-- The image format is inferred from the file suffix.
writeChannels :: FilePath -> Channels N4 Word8 -> IO ()
writeChannels = (. channelArray) . fmap Repa.runIL . Repa.writeImage

-- | Converts a 'Float'-based channel array to a 'Word8'-based one.
floatToByteChannels :: (NaturalNumber r)
                       => Channels r Float -> Channels r Word8
floatToByteChannels = mapChannels floatToByte

-- | Converts a 'Word8'-based channel array to a 'Float'-based one.
byteToFloatChannels :: (NaturalNumber r)
                       => Channels r Word8 -> Channels r Float
byteToFloatChannels = mapChannels byteToFloat

-- | Performs an arbitrary operation on the channel array of some
-- 'Channels'.
mapChannels :: (Repa.Elt n1, Repa.Elt n2, NaturalNumber r)
               => (n1 -> n2) -> Channels r n1 -> Channels r n2
mapChannels = mapChannelsArray . Repa.map

mapChannelsArray :: (Repa.Elt n1, Repa.Elt n2, NaturalNumber r)
                     => (Array DIM3 n1 -> Array DIM3 n2)
                     -> Channels r n1 -> Channels r n2
mapChannelsArray f (Channels array) = makeChannels . f $ array

-- | Removes an alpha channel from the specified 'Channels'.
removeAlphaChannel :: (Repa.Elt n) => Channels N4 n -> Channels N3 n
removeAlphaChannel =
  makeChannels . Repa.force . flip2 Repa.traverse decrChannel id . channelArray
  where
    {-# INLINE decrChannel #-}
    decrChannel (s :. _) = s :. 3

-- | Adds an alpha channel with the specified constant value.
addAlphaChannel :: (Repa.Elt n) => n -> Channels N3 n -> Channels N4 n
addAlphaChannel value =
  makeChannels . Repa.force .
  flip2 Repa.traverse incrChannel setAlpha . channelArray
  where
    {-# INLINE incrChannel #-}
    incrChannel (ds :. 3) = ds :. 4
    incrChannel sh        = sh
    {-# INLINE setAlpha #-}
    setAlpha _    (_ :. 3) = value
    setAlpha look coord    = look coord

-- | Checks whether the 'Channels' contain an alpha channel.
hasAlphaChannel :: forall n r . (Repa.Elt n, NaturalNumber r)
                   => Channels r n -> Bool
hasAlphaChannel _ = naturalNumberAsInt (undefined :: r) > 3

{-# INLINE flip2 #-}
flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c
