module Graphics.Forensics.Utilities
       ( zipWith3
       , mapM
       , r2c1d
       , r2c2d
       , r2c3d
       , c2r
       , liftC
       ) where

import Prelude hiding (zipWith3, mapM)

import Data.Array.Repa (Array(..), D, DIM1, DIM2, DIM3,
                        Source(..), Shape(..), (:.)(..), Z(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as Repa
import Data.Array.Repa.Repr.ForeignPtr (F)
import qualified Data.Array.Repa.Unsafe as Repa

import qualified Data.Array.CArray as CArray
import Data.Array.CArray (CArray)

import Foreign.Storable (Storable(..))
import Foreign.Storable.Complex ()
import System.IO.Unsafe (unsafePerformIO)

-- | zipWith3 for Repa
zipWith3 :: (Repa.Elt a, Repa.Elt b, Repa.Elt c, Repa.Elt d,
             Source r1 a, Source r2 b, Source r3 c, Shape sh) =>
            (a -> b -> c -> d) ->
            Array r1 sh a -> Array r2 sh b ->
            Array r3 sh c -> Array D sh d
zipWith3 f a b c =
  Repa.unsafeTraverse3 a b c (const . const) traverseFunc
  where
    traverseFunc get1 get2 get3 ix = f (get1 ix) (get2 ix) (get3 ix)

-- | mapM for Repa
mapM :: (Repa.Elt a, Repa.Elt b, Source r1 a, Shape sh, Monad m,
         Repa.Target D b) =>
        (a -> m b) -> Array r1 sh a -> m (Array D sh b)
mapM f a = do
  let mapped = Repa.map f a
      sh = extent a
  result <- sequence . Repa.toList $ mapped
  return $ Repa.fromList sh result

{- These functions can be used to convert Repa to CArray and back. -}

{-# INLINE r2c1d #-}
r2c1d :: (Storable e) => Array F DIM1 e -> CArray Int e
r2c1d array = unsafePerformIO $ do
  let x = Repa.size $ extent array
      ptr = Repa.toForeignPtr array
  CArray.unsafeForeignPtrToCArray ptr (0, x - 1)

{-# INLINE r2c2d #-}
r2c2d :: (Storable e) => Array F DIM2 e -> CArray (Int, Int) e
r2c2d array = unsafePerformIO $ do
  let (Z :. x :. y) = extent array
      ptr = Repa.toForeignPtr array
  CArray.unsafeForeignPtrToCArray ptr ((0, 0), (x - 1, y - 1))

{-# INLINE r2c3d #-}
r2c3d :: (Storable e) => Array F DIM3 e -> CArray (Int, Int, Int) e
r2c3d array = unsafePerformIO $ do
  let (Z :. x :. y :. z) = extent array
      ptr = Repa.toForeignPtr array
  CArray.unsafeForeignPtrToCArray ptr ((0, 0, 0), (x - 1, y - 1, z - 1))

{- TODO: Find out if this safe, or if the memory could be freed -}
{-# INLINE c2r #-}
c2r :: (Shape sh, Storable e, CArray.Ix ix) => sh -> CArray ix e -> Array F sh e
c2r sh !array =
  Repa.fromForeignPtr sh ptr
  where
    (_, ptr) = CArray.toForeignPtr array

{- Applies a given function on a CArray to a ForeignPtr Repa -}
liftC :: (Storable e, Shape sh, CArray.Ix ix) =>
         (Array F sh e -> CArray ix e) ->
         (CArray ix e -> CArray ix e) ->
         Array F sh e -> Array F sh e
liftC convert f !array = c2r (extent array) (f . convert $ array)