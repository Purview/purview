module Graphics.Forensics.Analysers where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Image

import Graphics.Forensics.Analyser.Test as Test

analysers :: [Analyser ByteImage]
analysers =
  [ Test.analyser
  ]
