module Test.Graphics.Forensics.Algorithms where

import Data.Array.Repa (DIM1, DIM2, DIM3, Array, Elt)
import Data.Array.Repa.Arbitrary
import Data.Array.Repa.Algorithms.Complex
import Graphics.Forensics.Algorithms

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testGroup :: Test
testGroup = $(testGroupGenerator)

prop_dft1d :: Array DIM1 Complex -> Bool
prop_dft1d a =
  (dft1d Inverse . dft1d Forward $ a) == a

prop_dft2d :: Array DIM2 Complex -> Bool
prop_dft2d a =
  (dft2d Inverse . dft2d Forward $ a) == a

prop_dft3d :: Array DIM3 Complex -> Bool
prop_dft3d a =
  (dft3d Inverse . dft3d Forward $  a) == a

complexToReal :: Complex -> Double
complexToReal = fst

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM1 a) where
  arbitrary = arbitrarySmallArray 512

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM2 a) where
  arbitrary = arbitrarySmallArray 128

instance (Elt a, Arbitrary a)
         => Arbitrary (Array DIM3 a) where
  arbitrary = arbitrarySmallArray 16
