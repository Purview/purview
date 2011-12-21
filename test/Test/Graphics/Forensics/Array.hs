module Test.Graphics.Forensics.Array where

import qualified Data.Array as Array
import Data.Array.Repa
import Data.Array.Repa.Arbitrary
import Data.Word

import Graphics.Forensics.Array

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_byteConversion :: Word8 -> Bool
prop_byteConversion byte =
  (floatToByte . byteToFloat $ byte) == byte

prop_arrayConversion :: Array DIM2 Word8 -> Bool
prop_arrayConversion array =
  (floatToByteArray . byteToFloatArray $ array) == array

prop_slicing :: Array DIM2 Int -> Bool
prop_slicing array =
  (glueArrays . sliceArray $ array) == array

prop_array1D :: Array DIM1 Int -> Bool
prop_array1D array =
  let dataArray = toDataArray array :: Array.Array Int Int
  in fromDataArray dataArray == array

prop_array2D :: Array DIM2 Int -> Bool
prop_array2D array =
  let dataArray = toDataArray array :: Array.Array (Int, Int) Int
  in fromDataArray dataArray == array

prop_array3D :: Array DIM3 Int -> Bool
prop_array3D array =
  let dataArray = toDataArray array :: Array.Array (Int, Int, Int) Int
  in fromDataArray dataArray == array

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM1 a) where
  arbitrary = arbitrarySmallArray 512

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM2 a) where
  arbitrary = arbitrarySmallArray 128

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM3 a) where
  arbitrary = arbitrarySmallArray 16
