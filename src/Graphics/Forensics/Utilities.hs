module Graphics.Forensics.Utilities
       (
         zipWith3
       ) where

import Prelude hiding (zipWith3)

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