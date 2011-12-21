module Graphics.Forensics.Color where

import qualified Data.Array.Repa as Repa
import qualified Data.Colour.SRGB as Colour
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed.Base

data RGBA n =
  RGBA
  { channelRed   :: !n
  , channelGreen :: !n
  , channelBlue  :: !n
  , channelAlpha :: !n
  }
  deriving (Show, Eq)

toSRGB :: RGBA n -> Colour.RGB n
toSRGB (RGBA r g b _) = Colour.RGB r g b

fromTransparentSRGB :: Num n => Colour.RGB n -> RGBA n
fromTransparentSRGB = fromSRGBAlpha 0

fromSRGBAlpha :: n -> Colour.RGB n -> RGBA n
fromSRGBAlpha a (Colour.RGB r g b) = RGBA r g b a

instance Repa.Elt a => Repa.Elt (RGBA a) where
  {-# INLINE touch #-}
  touch (RGBA r g b a) = do
    Repa.touch r
    Repa.touch g
    Repa.touch b
    Repa.touch a

  {-# INLINE zero #-}
  zero = RGBA Repa.zero Repa.zero Repa.zero Repa.zero
  {-# INLINE one #-}
  one = RGBA Repa.one Repa.one Repa.one Repa.one

data instance MVector s (RGBA a) =
  MV_RGBA {-# UNPACK #-} !Int
  !(MVector s a) !(MVector s a) !(MVector s a) !(MVector s a)

data instance Vector (RGBA a) =
  V_RGBA {-# UNPACK #-} !Int
  !(Vector a) !(Vector a) !(Vector a) !(Vector a)

instance Unbox a => Unbox (RGBA a)

instance Unbox a => M.MVector MVector (RGBA a) where
  {-# INLINE basicLength #-}
  basicLength (MV_RGBA n _ _ _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i m (MV_RGBA _ rs gs bs as) =
    MV_RGBA m
    (M.basicUnsafeSlice i m rs)
    (M.basicUnsafeSlice i m gs)
    (M.basicUnsafeSlice i m bs)
    (M.basicUnsafeSlice i m as)
  {-# INLINE basicOverlaps #-}
  basicOverlaps
    (MV_RGBA _ rs1 gs1 bs1 as1)
    (MV_RGBA _ rs2 gs2 bs2 as2) =
    M.basicOverlaps rs1 rs2 ||
    M.basicOverlaps gs1 gs2 ||
    M.basicOverlaps bs1 bs2 ||
    M.basicOverlaps as1 as2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    rs <- M.basicUnsafeNew n
    gs <- M.basicUnsafeNew n
    bs <- M.basicUnsafeNew n
    as <- M.basicUnsafeNew n
    return $ MV_RGBA n rs gs bs as
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (RGBA r g b a) = do
    rs <- M.basicUnsafeReplicate n r
    gs <- M.basicUnsafeReplicate n g
    bs <- M.basicUnsafeReplicate n b
    as <- M.basicUnsafeReplicate n a
    return $ MV_RGBA n rs gs bs as
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_RGBA _ rs gs bs as) i = do
    r <- M.basicUnsafeRead rs i
    g <- M.basicUnsafeRead gs i
    b <- M.basicUnsafeRead bs i
    a <- M.basicUnsafeRead as i
    return $ RGBA r g b a
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_RGBA _ rs gs bs as) i (RGBA r g b a) = do
    M.basicUnsafeWrite rs i r
    M.basicUnsafeWrite gs i g
    M.basicUnsafeWrite bs i b
    M.basicUnsafeWrite as i a
  {-# INLINE basicClear #-}
  basicClear (MV_RGBA _ rs gs bs as) = do
    M.basicClear rs
    M.basicClear gs
    M.basicClear bs
    M.basicClear as
  {-# INLINE basicSet #-}
  basicSet (MV_RGBA _ rs gs bs as) (RGBA r g b a) = do
    M.basicSet rs r
    M.basicSet gs g
    M.basicSet bs b
    M.basicSet as a
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy
    (MV_RGBA _ rs1 gs1 bs1 as1)
    (MV_RGBA _ rs2 gs2 bs2 as2) = do
    M.basicUnsafeCopy rs1 rs2
    M.basicUnsafeCopy gs1 gs2
    M.basicUnsafeCopy bs1 bs2
    M.basicUnsafeCopy as1 as2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove
    (MV_RGBA _ rs1 gs1 bs1 as1)
    (MV_RGBA _ rs2 gs2 bs2 as2) = do
    M.basicUnsafeMove rs1 rs2
    M.basicUnsafeMove gs1 gs2
    M.basicUnsafeMove bs1 bs2
    M.basicUnsafeMove as1 as2
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MV_RGBA n rs gs bs as) m = do
    rs' <- M.basicUnsafeGrow rs m
    gs' <- M.basicUnsafeGrow gs m
    bs' <- M.basicUnsafeGrow bs m
    as' <- M.basicUnsafeGrow as m
    return $ MV_RGBA (m + n) rs' gs' bs' as'

instance Unbox a => G.Vector Vector (RGBA a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_RGBA n rs gs bs as) = do
    rs' <- G.basicUnsafeFreeze rs
    gs' <- G.basicUnsafeFreeze gs
    bs' <- G.basicUnsafeFreeze bs
    as' <- G.basicUnsafeFreeze as
    return $ V_RGBA n rs' gs' bs' as'
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_RGBA n rs gs bs as) = do
    rs' <- G.basicUnsafeThaw rs
    gs' <- G.basicUnsafeThaw gs
    bs' <- G.basicUnsafeThaw bs
    as' <- G.basicUnsafeThaw as
    return $ MV_RGBA n rs' gs' bs' as'
  {-# INLINE basicLength #-}
  basicLength (V_RGBA n _ _ _ _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i m (V_RGBA _ rs gs bs as) =
    V_RGBA m
    (G.basicUnsafeSlice i m rs)
    (G.basicUnsafeSlice i m gs)
    (G.basicUnsafeSlice i m bs)
    (G.basicUnsafeSlice i m as)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_RGBA _ rs gs bs as) i = do
    r <- G.basicUnsafeIndexM rs i
    g <- G.basicUnsafeIndexM gs i
    b <- G.basicUnsafeIndexM bs i
    a <- G.basicUnsafeIndexM as i
    return $ RGBA r g b a
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy
    (MV_RGBA _ rs1 gs1 bs1 as1)
    (V_RGBA _ rs2 gs2 bs2 as2) = do
    G.basicUnsafeCopy rs1 rs2
    G.basicUnsafeCopy gs1 gs2
    G.basicUnsafeCopy bs1 bs2
    G.basicUnsafeCopy as1 as2
  {-# INLINE elemseq #-}
  elemseq _ (RGBA r g b a) =
    G.elemseq (undefined :: Vector a) r .
    G.elemseq (undefined :: Vector a) g .
    G.elemseq (undefined :: Vector a) b .
    G.elemseq (undefined :: Vector a) a
