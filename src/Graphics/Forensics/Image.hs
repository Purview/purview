-- | Allows images to be treated like color matrices
module Graphics.Forensics.Image
       ( -- * Image
         Image
         -- * File system
       , readImage
       , writeImage
       ) where

import Prelude hiding (lookup)

import Data.Colour.SRGB
import Data.Array.Repa (Array, DIM2, (:.)(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.DevIL as Repa
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed.Base

-- | An 'Image' is a 2-dimensional matrix of floating-point 'RGB' colors
type Image = Array DIM2 (RGB Float)

-- | Loads an 'Image' matrix from a file. Very many image formats are
-- supported.
readImage :: FilePath -> IO Image
readImage path = do
  image <- Repa.runIL . Repa.readImage $ path
  return $ Repa.traverse image dropChannel readRGBColor
  where
    dropChannel (s :. _) = s
    readRGBColor lookup coord =
      let c i = (/ 255) . fromIntegral . lookup $ coord :. i
      in RGB (c 0) (c 1) (c 2)

-- | Saves the specified 'Image' matrix to the specified path. The
-- image format is inferred from the file suffix.
writeImage :: FilePath -> Image -> IO ()
writeImage path image =
  Repa.runIL . Repa.writeImage path $
  Repa.traverse image addChannel writeRGBColor
  where
    addChannel s = s :. 3
    writeRGBColor lookup (coord :. c) =
      (* 255) . truncate . clamp 0 1 . channel c . lookup $ coord
    channel 0 = channelRed
    channel 1 = channelBlue
    channel 2 = channelGreen
    channel _ = undefined

clamp :: (Num n, Ord n)=> n -> n -> n -> n
clamp low hi num
  | num < low = low
  | num > hi  = hi
  | otherwise = num

instance Repa.Elt a => Repa.Elt (RGB a) where
  touch (RGB r g b) = do
    Repa.touch r
    Repa.touch g
    Repa.touch b

  zero = RGB Repa.zero Repa.zero Repa.zero
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
