module RayT.VectorTests 
  ( vectorTests
  ) where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.Vector

vectorTests :: Test
vectorTests =
    testGroup "Vector addition"
    [
     testCase 
        "einfache HUNIT"
        simpleAssert
     ,testProperty 
        "the 0-vector is the additive neutral element"
        zeroIsNeutral
     ,testProperty 
        "vector addition is commutative"
        vectorAdditionIsCommutative
    ]
-- A recommended way of creating HUnit tests. Such tests are easy to integrate
-- with test-framework (see MainTestSuite.hs)
simpleAssert :: Assertion
simpleAssert =
    5 @=? 5

simpleCheck :: Int -> Bool
simpleCheck i =
  i < 0 || i > 10000 ||
    (i + i == 2*i)

zeroIsNeutral :: R3 -> Bool
zeroIsNeutral v = 
    v + 0 == v &&
    0 + v == v

vectorAdditionIsCommutative :: R3 -> R3 -> Bool
vectorAdditionIsCommutative v v' = 
    v + v' == v' + v

-- * instances

instance Arbitrary a => Arbitrary (Vector3 a) where
    arbitrary = do
      [a,b,c] <- vector 3
      return $ Vec3 (a,b,c)
    shrink _ = []