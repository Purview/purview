module Test.Graphics.Forensics.Channels where

import Data.Array.Repa
import Data.Array.Repa.Arbitrary
import Data.Word

import Graphics.Forensics.Channels

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_channelConversion :: Channels Word8 -> Bool
prop_channelConversion channels =
  (floatToByteChannels . byteToFloatChannels $ channels) == channels

instance (Elt a, Arbitrary a)
         => Arbitrary (Channels a) where
  arbitrary = do
    isRgba <- arbitrary
    sh0 <- arbitrarySmallShape 128
    let sh = if isRgba then sh0 :. 4 else sh0 :. 3
        constr = if isRgba then RGBAChannels else RGBChannels
    list <- arbitraryListOfLength . size $ sh
    return $ constr . fromList sh $ list
