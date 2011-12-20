-- | Allows images to be treated like color matrices
module Graphics.Forensics.Image
       ( -- * Image
         Image
         -- ** File system
       , readImage
       , writeImage
         -- * Channels
       , RGBChannels
         -- ** File system
       , readChannels
       , writeChannels
         -- ** Conversion
       , splitChannels
       , mergeChannels
         -- * Formats
       , floatToByteImage
       , byteToFloatImage
       , floatToByteMatrix
       , byteToFloatMatrix
       ) where

import Prelude hiding (lookup)

import Data.Colour.SRGB
import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed.Base
import Data.Word

-- | An 'Image' is a 2-dimensional matrix of 'RGB' colors
type Image n = Array DIM2 (RGB n)

-- | A set of 'RGBChannels' is a 3-dimensional matrix of numbers, where
-- the matrix's first two index coordinates specify the pixel, and
-- the third index coordinate specifies the color channel index.
type RGBChannels n = Array DIM3 n

-- | Loads an 'Image' matrix from a file. Very many image formats are
-- supported.
readImage :: FilePath -> IO (Image Word8)
readImage = fmap mergeChannels . readChannels

-- | Saves the specified 'Image' matrix to the specified path.
-- The image format is inferred from the file suffix.
writeImage :: FilePath -> Image Word8 -> IO ()
writeImage = (. splitChannels) . writeChannels

-- | Loads a channel matrix from a file. Very many image formats are
-- supported.
readChannels :: FilePath -> IO (RGBChannels Word8)
readChannels = fmap removeAlphaChannel . Repa.runIL . Repa.readImage

-- | Saves the specified 'RGBChannels' matrix to the specified path.
-- The image format is inferred from the file suffix.
writeChannels :: FilePath -> RGBChannels Word8 -> IO ()
writeChannels = (. addAlphaChannel 255) . fmap Repa.runIL . Repa.writeImage

-- | Converts a 96-bit floating point color image to a 24-bit color image
floatToByteImage :: Image Float -> Image Word8
floatToByteImage = Repa.map $ mapColor floatToByte

-- | Converts a 24-bit color image to a 96-bit floating point color image
byteToFloatImage :: Image Word8 -> Image Float
byteToFloatImage = Repa.map $ mapColor byteToFloat

-- | Converts a matrix of floats between 0 and 1 to a matrix of bytes
floatToByteMatrix :: Repa.Shape s => Array s Float -> Array s Word8
floatToByteMatrix = Repa.map floatToByte

-- | Converts a matrix of bytes to a matrix of floats between 0 and 1
byteToFloatMatrix :: Repa.Shape s => Array s Word8 -> Array s Float
byteToFloatMatrix = Repa.map byteToFloat

-- | Separates an image into a 3D matrix, where the first two
-- coordinates specify the pixel coordinate, and the third coordinate
-- specifies the channel index.
splitChannels :: (Repa.Elt n) => Image n -> RGBChannels n
splitChannels =
  flip2 Repa.traverse addChannel writeRGBColor
  where
    {-# INLINE addChannel #-}
    addChannel s = s :. 3
    {-# INLINE writeRGBColor #-}
    writeRGBColor lookup (coord :. c) = channel c . lookup $ coord
    {-# INLINE channel #-}
    channel 0 = channelRed
    channel 1 = channelGreen
    channel 2 = channelBlue
    channel _ = undefined

-- | Merges a 3D channel matrix into an SRGB color image.
mergeChannels :: (Repa.Elt n) => RGBChannels n -> Image n
mergeChannels =
  flip2 Repa.traverse dropChannel readRGBColor
  where
    {-# INLINE dropChannel #-}
    dropChannel (s :. _) = s
    {-# INLINE readRGBColor #-}
    readRGBColor lookup coord =
      let color i = lookup $ coord :. i
      in RGB (color 0) (color 1) (color 2)

{-# INLINE removeAlphaChannel #-}
removeAlphaChannel :: (Repa.Elt n) => RGBChannels n -> RGBChannels n
removeAlphaChannel =
  Repa.force . flip2 Repa.traverse decrChannel id
  where
    {-# INLINE decrChannel #-}
    decrChannel (s :. _) = s :. 3

{-# INLINE addAlphaChannel #-}
addAlphaChannel :: (Repa.Elt n) => n -> RGBChannels n -> RGBChannels n
addAlphaChannel value =
  until hasAlphaChannel $
  Repa.force . flip2 Repa.traverse incrChannel setAlpha
  where
    {-# INLINE incrChannel #-}
    incrChannel sh @ (ds :. chanCount)
      | chanCount < 4 = ds :. 4
      | otherwise     = sh
    {-# INLINE setAlpha #-}
    setAlpha _      (_ :. 3) = value
    setAlpha lookup coord    = lookup coord

{-# INLINE hasAlphaChannel #-}
hasAlphaChannel :: (Repa.Elt n) => RGBChannels n -> Bool
hasAlphaChannel (Repa.Array (Z :. _ :. _ :. d) _) = d > 3

{-# INLINE flip2 #-}
flip2 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip2 f b c a = f a b c

{-# INLINE byteToFloat #-}
byteToFloat :: Word8 -> Float
byteToFloat = (/ 255) . fromIntegral

{-# INLINE floatToByte #-}
floatToByte :: Float -> Word8
floatToByte = round . (* 255) . clamp 0 1

{-# INLINE mapColor #-}
mapColor :: (a -> b) -> RGB a -> RGB b
mapColor f (RGB r g b) = RGB (f r) (f g) (f b)

{-# INLINE clamp #-}
clamp :: Ord n => n -> n -> n -> n
clamp low hi num
  | num < low = low
  | num > hi  = hi
  | otherwise = num

instance Repa.Elt a => Repa.Elt (RGB a) where
  {-# INLINE touch #-}
  touch (RGB r g b) = do
    Repa.touch r
    Repa.touch g
    Repa.touch b

  {-# INLINE zero #-}
  zero = RGB Repa.zero Repa.zero Repa.zero
  {-# INLINE one #-}
  one = RGB Repa.one Repa.one Repa.one

data instance MVector s (RGB a) =
  MV_RGB {-# UNPACK #-} !Int !(MVector s a) !(MVector s a) !(MVector s a)

data instance Vector (RGB a) =
  V_RGB {-# UNPACK #-} !Int !(Vector a) !(Vector a) !(Vector a)

instance Unbox a => Unbox (RGB a)

instance Unbox a => M.MVector MVector (RGB a) where
  {-# INLINE basicLength #-}
  basicLength (MV_RGB n _ _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i m (MV_RGB _ rs gs bs) =
    MV_RGB m
    (M.basicUnsafeSlice i m rs)
    (M.basicUnsafeSlice i m gs)
    (M.basicUnsafeSlice i m bs)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_RGB _ rs1 gs1 bs1) (MV_RGB _ rs2 gs2 bs2) =
    M.basicOverlaps rs1 rs2 ||
    M.basicOverlaps gs1 gs2 ||
    M.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    rs <- M.basicUnsafeNew n
    gs <- M.basicUnsafeNew n
    bs <- M.basicUnsafeNew n
    return $ MV_RGB n rs gs bs
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (RGB r g b) = do
    rs <- M.basicUnsafeReplicate n r
    gs <- M.basicUnsafeReplicate n g
    bs <- M.basicUnsafeReplicate n b
    return $ MV_RGB n rs gs bs
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_RGB _ rs gs bs) i = do
    r <- M.basicUnsafeRead rs i
    g <- M.basicUnsafeRead gs i
    b <- M.basicUnsafeRead bs i
    return $ RGB r g b
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_RGB _ rs gs bs) i (RGB r g b) = do
    M.basicUnsafeWrite rs i r
    M.basicUnsafeWrite gs i g
    M.basicUnsafeWrite bs i b
  {-# INLINE basicClear #-}
  basicClear (MV_RGB _ rs gs bs) = do
    M.basicClear rs
    M.basicClear gs
    M.basicClear bs
  {-# INLINE basicSet #-}
  basicSet (MV_RGB _ rs gs bs) (RGB r g b) = do
    M.basicSet rs r
    M.basicSet gs g
    M.basicSet bs b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_RGB _ rs1 gs1 bs1) (MV_RGB _ rs2 gs2 bs2) = do
    M.basicUnsafeCopy rs1 rs2
    M.basicUnsafeCopy gs1 gs2
    M.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MV_RGB _ rs1 gs1 bs1) (MV_RGB _ rs2 gs2 bs2) = do
    M.basicUnsafeMove rs1 rs2
    M.basicUnsafeMove gs1 gs2
    M.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_RGB n rs gs bs) m = do
    rs' <- M.basicUnsafeGrow rs m
    gs' <- M.basicUnsafeGrow gs m
    bs' <- M.basicUnsafeGrow bs m
    return $ MV_RGB (m + n) rs' gs' bs'

instance Unbox a => G.Vector Vector (RGB a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_RGB n rs gs bs) = do
    rs' <- G.basicUnsafeFreeze rs
    gs' <- G.basicUnsafeFreeze gs
    bs' <- G.basicUnsafeFreeze bs
    return $ V_RGB n rs' gs' bs'
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_RGB n rs gs bs) = do
    rs' <- G.basicUnsafeThaw rs
    gs' <- G.basicUnsafeThaw gs
    bs' <- G.basicUnsafeThaw bs
    return $ MV_RGB n rs' gs' bs'
  {-# INLINE basicLength #-}
  basicLength (V_RGB n _ _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i m (V_RGB _ rs gs bs) =
    V_RGB m
    (G.basicUnsafeSlice i m rs)
    (G.basicUnsafeSlice i m gs)
    (G.basicUnsafeSlice i m bs)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_RGB _ rs gs bs) i = do
    r <- G.basicUnsafeIndexM rs i
    g <- G.basicUnsafeIndexM gs i
    b <- G.basicUnsafeIndexM bs i
    return $ RGB r g b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MV_RGB _ rs1 gs1 bs1) (V_RGB _ rs2 gs2 bs2) = do
    G.basicUnsafeCopy rs1 rs2
    G.basicUnsafeCopy gs1 gs2
    G.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq #-}
  elemseq _ (RGB r g b) =
    G.elemseq (undefined :: Vector a) r .
    G.elemseq (undefined :: Vector a) g .
    G.elemseq (undefined :: Vector a) b
