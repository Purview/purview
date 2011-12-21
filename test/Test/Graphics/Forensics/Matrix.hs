module Test.Graphics.Forensics.Matrix where

import Data.Array.Repa
import Data.Array.Repa.Arbitrary
import Data.Word

import Graphics.Forensics.Matrix

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_losslessByte :: Word8 -> Bool
prop_losslessByte byte =
  (floatToByte . byteToFloat $ byte) == byte

prop_losslessMatrix :: Array DIM2 Word8 -> Bool
prop_losslessMatrix matrix =
  (floatToByteMatrix . byteToFloatMatrix $ matrix) == matrix

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM2 a) where
  arbitrary = arbitrarySmallArray 128
