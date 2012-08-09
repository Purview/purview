module Graphics.Forensics.Utilities
       (
         zipWith3
       , mapM
       ) where

import Prelude hiding (zipWith3, mapM)

import Data.Array.Repa (Array(..), D, Source(..), Shape(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa

-- | zipWith3 for Data.Array.Repa
zipWith3 :: (Repa.Elt a, Repa.Elt b, Repa.Elt c, Repa.Elt d,
             Source r1 a, Source r2 b, Source r3 c, Shape sh) =>
            (a -> b -> c -> d) ->
            Array r1 sh a -> Array r2 sh b ->
            Array r3 sh c -> Array D sh d
zipWith3 = ((Repa.zipWith id .) .) . Repa.zipWith

mapM :: (Repa.Elt a, Repa.Elt b, Source r1 a, Shape sh, Monad m,
         Repa.Target D b) =>
        (a -> m b) -> Array r1 sh a -> m (Array D sh b)
mapM f a = do
  let mapped = Repa.map f a
  let sh = extent a
  result <- sequence . Repa.toList $ mapped
  return $ Repa.fromList sh result