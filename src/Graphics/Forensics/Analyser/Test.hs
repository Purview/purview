module Graphics.Forensics.Analyser.Test where

import Control.Monad

import Graphics.Forensics.Analyser
import Graphics.Forensics.Image
import Graphics.Forensics.Report
import Graphics.Forensics.Shape

analyser :: Analyser ByteImage
analyser =
  Analyser
  { analyse = testAnalyse
  , name = "test"
  , author = "David Flemström"
  , version = readVersion "0.1"
  }

testAnalyse :: ByteImage -> Analysis ()
testAnalyse img = task "Analysis" 3 $ do
  task "Content report" 2 $ do
    task "Image report" 1 $ do
      reportInfo "Input image" . reportImage $ img
    step
    task "Shape report" 1 $ do
      reportWarning "Suspicious" . reportShape $ rectangle 43 24
  step
  task "Text report" 5 $ do
    whisper "debug"
    step
    inform "info"
    step
    warn "warning"
    step
    yell "error"
    step
    panic "critical"
  step
  task "Long thing" 100 $ forM_ [1..(99 :: Int)] $ const step
