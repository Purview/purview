module Graphics.Forensics.Analysers where

import Graphics.Forensics.Analyser
import Graphics.Forensics.Image
import Graphics.Forensics.Analyser.LuminanceGradient as LG
import Graphics.Forensics.Analyser.Test as Test
import Graphics.Forensics.Analyser.LocalCFA as LCFA
analysers :: [Analyser ByteImage]
analysers =
  [ Test.analyser,
    LG.analyser,
    LCFA.analyser
  ]
