-- | Provides a system for analysing image data.
module Graphics.Forensics.Analyser
       ( -- * Analyser
         Analyser(..)
         -- * Output
       , Analysis
       ) where

import Control.Monad.Progress

import Data.Text (Text)
import Data.Version (Version)

import Graphics.Forensics.Report

-- | A running analysis.
type Analysis = Progress Text Report

{-| An 'Analyser' that analyses some type of item.

The analyser can yield 'Progress' indications during its execution, and
incrementally write to a 'Report' that is to be considered the result
of the analysis.
-}
class Analyser a i | a -> i where
  -- | Analyse the given item with this analyser
  analyse :: a      -- ^ The analyser data
             -> i   -- ^ The item to analyse
             -> Analysis

  -- | The human-readable name of this analyser
  name :: a -> Text

  -- | The author of this analyser
  author :: a -> Text
  author _ = ""

  -- | The version of this analyser
  version :: a -> Version
  version _ = read "1.0"
