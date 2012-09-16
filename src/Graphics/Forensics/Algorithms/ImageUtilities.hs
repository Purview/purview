module Graphics.Forensics.Algorithms.ImageUtilities
       ( scaleArray
       , normalize
       ) where

import Data.Array.Repa (Array(..), D, foldAllS, Shape(..), Source(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Eval (Elt(..))
import Data.Array.Repa.Repr.Unboxed (Unbox)

scaleArray :: (Source r1 e, Shape sh, Real e) =>
              e -> e -> e -> e ->
             Array r1 sh e -> Array D sh Float
scaleArray oMin oMax scMin scMax !image =
  Repa.map norm image
  where
    norm value = (realToFrac (value - oMin) / realToFrac oMax) *
                 realToFrac scMax + realToFrac scMin

normalize :: (Source r1 e, Shape sh, Real e, Unbox e, Elt e) =>
             Array r1 sh e -> Array D sh Float
normalize a =
  scaleArray aMin aMax 0 1 a
  where
    aMin = foldAllS min 100000 a
    aMax = foldAllS max 0 a