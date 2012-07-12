-- | Contains utilities for images in Repa array representation
module Graphics.Forensics.Utils
       (
         copyUnboxedP
       ) where

import qualified Data.Array.Repa (..) as Repa

-- | copyP with a more specific type for easier use
copyUnboxedP :: (Shape sh, Load r1 sh e, Monad m, U.Unbox e) =>
                Array r1 sh e -> m (Array U sh e)
copyUnboxedP = Repa.copyP

copyForeignPtrP :: (Shape sh, Load r1 sh e, Monad m, Storable e) =>
                Array r1 sh e -> m (Array F sh e)
copyForeignPtrP = Repa.copyP