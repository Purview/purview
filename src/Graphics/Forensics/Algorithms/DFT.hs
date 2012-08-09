module Graphics.Forensics.Algorithms.DFT
       ( idftP
       , idftS
       , dftP
       , dftS
       , calcRootsOfUnityP
       , calcRootsOfUnityS
       , calcInverseRootsOfUnityP
       , calcInverseRootsOfUnityS
       , fft1dP
       , fft2dP
       , fft3dP
       , Mode (..)
       , realToComplex
       , complexRound
       ) where

import Prelude as P hiding (map)
import Data.Array.Repa
import Data.Array.Repa.Algorithms.DFT
import Data.Array.Repa.Algorithms.DFT.Roots
import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.Complex

-- This file contains rewritten functions of
-- Data.Array.Repa.Algorithms that use computeS instead of computeP.
-- It also exports many of the built-in functions for
-- Graphics.Forensics.Algorithms to re-export.

calcRootsOfUnityS :: (Shape sh) =>
                     (sh :. Int) -> Array U (sh :. Int) Complex
calcRootsOfUnityS sh@(_ :. n) =
  computeS $ fromFunction sh f
  where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) =
      ( cos  (2 * pi * (fromIntegral i) / len),
       -sin  (2 * pi * (fromIntegral i) / len))
    len = fromIntegral n

calcInverseRootsOfUnityS :: (Shape sh) =>
                            (sh :. Int) -> Array U (sh :. Int) Complex
calcInverseRootsOfUnityS sh@(_ :. n) =
  computeS $ fromFunction sh f
  where
    f :: Shape sh => (sh :. Int) -> Complex
    f (_ :. i) =
      (cos  (2 * pi * (fromIntegral i) / len),
       sin  (2 * pi * (fromIntegral i) / len))
    len = fromIntegral n

dftS :: (Shape sh) =>
        Array U (sh :. Int) Complex ->
        Array U (sh :. Int) Complex
dftS v = dftWithRootsS (calcRootsOfUnityS $ extent v) v
{-# INLINE dftS #-}

idftS :: (Shape sh) =>
         Array U (sh :. Int) Complex ->
         Array U (sh :. Int) Complex
idftS v =
  computeS $ map (/ scale) roots
  where
    (_ :. len) = extent v
    scale = (fromIntegral len, 0)
    rofu = calcInverseRootsOfUnityS (extent v)
    roots = dftWithRootsS rofu v
{-# INLINE idftS #-}

dftWithRootsS :: (Shape sh) =>
                 Array U (sh :. Int) Complex ->
                 Array U (sh :. Int) Complex ->
                 Array U (sh :. Int) Complex
dftWithRootsS rofu arr
  | _ :. rLen <- extent rofu
  , _ :. vLen <- extent arr
  , rLen /= vLen =
    error $ "dftWithRoots: length of vector (" P.++ show vLen P.++ ")"
    P.++ " does not match the length of the roots (" P.++ show rLen P.++ ")"
  | otherwise =
    computeS $ traverse arr id (\_ k -> dftWithRootsSingleS rofu arr k)

realToComplex :: (Real a) => a -> Complex
realToComplex x =
  (realToFrac x, 0.0)

complexRound :: Complex -> Complex
complexRound (a, b) =
  (fromInteger $ round a, fromInteger $ round b)