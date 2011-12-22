module Test.Graphics.Forensics.Channels where

import Data.Array.Repa
import Data.Array.Repa.Arbitrary
import Data.Word

import Graphics.Forensics.Channels

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import TypeLevel.NaturalNumber

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_channelConversion :: Channels N4 Word8 -> Bool
prop_channelConversion channels =
  (floatToByteChannels . byteToFloatChannels $ channels) == channels

instance (Elt a, Arbitrary a)
         => Arbitrary (Channels N4 a) where
  arbitrary = do
    sh0 <- arbitrarySmallShape 128
    let sh = sh0 :. 4
    list <- arbitraryListOfLength . size $ sh
    return . makeChannels . fromList sh $ list
