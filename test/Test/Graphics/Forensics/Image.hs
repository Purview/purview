module Test.Graphics.Forensics.Image where

import Data.Word

import Graphics.Forensics.Color
import Graphics.Forensics.Image

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.Graphics.Forensics.Array ()

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_imageConversion :: Image Word8 -> Bool
prop_imageConversion img =
  (floatToByteImage . byteToFloatImage $ img) == img

prop_channelSplitting :: Image Word8 -> Bool
prop_channelSplitting img =
  (mergeChannels . splitChannels $ img) == img

instance Arbitrary a => Arbitrary (RGBA a) where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    a <- arbitrary
    return $ RGBA r g b a
