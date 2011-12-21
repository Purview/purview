module Test.Graphics.Forensics.Image where

import Data.Word

import Graphics.Forensics.Color
import Graphics.Forensics.Image

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Graphics.Forensics.Matrix ()

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_losslessImage :: Image Word8 -> Bool
prop_losslessImage img =
  (floatToByteImage . byteToFloatImage $ img) == img

instance Arbitrary a => Arbitrary (RGBA a) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    a <- arbitrary
    return $ RGBA r g b a
