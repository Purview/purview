module Graphics.Forensics.Analyser.LocalCFA where

import Control.Monad

import Graphics.Forensics.Analyser
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Shape

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = localCFAAnalyse
  , name = "localcfa"
  , author = "David Flemström & Moritz Roth"
  , version = readVersion "0.1"
  }

localCFAAnalyse :: ByteImage -> Analysis ()
localCFAAnalyse = undefined