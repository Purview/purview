module Test.Graphics.Forensics.Image where

import Data.Array.Repa
import Data.Array.Repa.Arbitrary
import Data.Colour.SRGB
import Data.Word

import Graphics.Forensics.Image

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_losslessImage :: Image Word8 -> Bool
prop_losslessImage img =
  (floatToByteImage . byteToFloatImage $ img) == img

prop_losslessMatrix :: Array DIM2 Word8 -> Bool
prop_losslessMatrix matrix =
  (floatToByteMatrix . byteToFloatMatrix $ matrix) == matrix

instance Arbitrary a => Arbitrary (RGB a) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ RGB r g b

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM2 a) where
  arbitrary = arbitrarySmallArray 128
