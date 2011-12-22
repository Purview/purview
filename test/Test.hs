import Test.Graphics.Forensics.Algorithms as Algorithms
import Test.Graphics.Forensics.Array as Array
import Test.Graphics.Forensics.Channels as Channels
import Test.Graphics.Forensics.Image as Image

import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Algorithms.testGroup
  , Array.testGroup
  , Channels.testGroup
  , Image.testGroup
  ]
