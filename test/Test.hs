
import Test.Graphics.Forensics.Image as Image

import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Image.testGroup
  ]
