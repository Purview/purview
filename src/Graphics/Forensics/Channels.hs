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

import Data.Array.Repa (Array(..), DIM3, (:.)(..), extent, Source(..), D)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import Data.Array.Repa.Repr.Unboxed (U, Unbox)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Word

import TypeLevel.NaturalNumber

import Graphics.Forensics.Array

-- | A set of 'Channels' is a 3-dimensional array of numbers, where
-- the array's first two index coordinates specify the pixel, and
-- the third index coordinate specifies the color channel index.
newtype Channels rep r n = Channels (Array rep DIM3 n)
                         deriving (Eq)

channelArray :: Channels rep r n -> Array rep DIM3 n
channelArray (Channels a) = a

-- | Safely constructs 'Channels' from an array by checking that the
-- correct number of channels are present.
makeChannels :: forall n r . (Repa.Elt n, NaturalNumber r) => Repa.Source rep n
                => Array rep DIM3 n -> Channels rep r n
makeChannels arr =
  let (_ :. nr) = extent arr in
  if nr == naturalNumberAsInt (undefined :: r)
  then Channels arr
  else error "Array extent doesn't match requested channel count"

-- | Loads a channel array from a file. Very many image formats are
-- supported. The channels are guaranteed to be in RGBA format.
readChannels :: FilePath -> IO (Channels U N4 Word8)
readChannels path = do
  image <- Repa.runIL $ Repa.readImage path
  let imgArray = imageArray image
  unboxed <- Repa.copyP imgArray
  return (makeChannels unboxed)

imageArray :: Repa.Image -> Repa.Array F DIM3 Word8
imageArray (Repa.RGBA img) = img
imageArray (Repa.RGB img) = img
imageArray (Repa.Grey _) = undefined

-- | Saves the specified 'Channels' array to the specified path.
-- The image format is inferred from the file suffix.
writeChannels :: (Repa.Source r Word8) =>
                 FilePath -> Channels r N4 Word8 -> IO ()
writeChannels path channels = do
  foreignPtrArray <- Repa.copyP $ channelArray channels
  let outImage = Repa.RGBA (foreignPtrArray)
  Repa.runIL $ Repa.writeImage path outImage

-- | Converts a 'Float'-based channel array to a 'Word8'-based one.
floatToByteChannels :: (NaturalNumber r, Repa.Source rep Float)
                       => Channels rep r Float -> Channels D r Word8
floatToByteChannels = mapChannels floatToByte

-- | Converts a 'Word8'-based channel array to a 'Float'-based one
byteToFloatChannels :: (NaturalNumber r, Repa.Source rep Word8)
                       => Channels rep r Word8 -> Channels D r Float
byteToFloatChannels = mapChannels byteToFloat

-- | Performs an arbitrary operation on the channel array of some
-- 'Channels'.
mapChannels :: (Unbox n1, Unbox n2, NaturalNumber r, Repa.Source rep n1,
                Repa.Source D n2, Repa.Elt n2)
               => (n1 -> n2) -> Channels rep r n1 -> Channels D r n2
mapChannels = mapChannelsArray . Repa.map

mapChannelsArray :: (Unbox n1, Unbox n2, NaturalNumber r, Repa.Source rep n1,
                     Repa.Source rep2 n2, Repa.Elt n2)
                     => (Array rep DIM3 n1 -> Array rep2 DIM3 n2)
                     -> Channels rep r n1 -> Channels rep2 r n2
mapChannelsArray f (Channels array) = makeChannels . f $ array

-- | Removes an alpha channel from the specified 'Channels'.
removeAlphaChannel :: (Repa.Elt n, Unbox n, Repa.Source U n) =>
                      Channels U N4 n -> Channels U N3 n
removeAlphaChannel =
  makeChannels . Repa.computeUnboxedS .
  flip2 Repa.traverse decrChannel id . channelArray
  where
    {-# INLINE decrChannel #-}
    decrChannel (s :. _) = s :. 3

-- | Adds an alpha channel with the specified constant value.
addAlphaChannel :: (Repa.Elt n, Unbox n, Repa.Source U n) =>
                   n -> Channels U N3 n -> Channels U N4 n
addAlphaChannel value =
  makeChannels . Repa.computeUnboxedS .
  flip2 Repa.traverse incrChannel setAlpha . channelArray
  where
    {-# INLINE incrChannel #-}
    incrChannel (ds :. 3) = ds :. 4
    incrChannel sh        = sh
    {-# INLINE setAlpha #-}
    setAlpha _    (_ :. 3) = value
    setAlpha look coord    = look coord

-- | Checks whether the 'Channels' contain an alpha channel.
hasAlphaChannel :: forall n r . (Unbox n, NaturalNumber r) => Repa.Source U n
                   => Channels U r n -> Bool
hasAlphaChannel _ = naturalNumberAsInt (undefined :: r) > 3

{-# INLINE flip2 #-}
flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c
