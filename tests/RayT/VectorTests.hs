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
  testGroup "Vector tests"
  [
    testGroup "Vector addition"
    [
     testCase 
        "einfache HUNIT"
        simpleAssert
     ,testProperty 
        "the 0-vector is the additive neutral element"
        ((\v -> v + 0 == v && 0 + v == v)
          :: R3 -> Bool)
     ,testProperty 
        "vector addition is commutative"
        ((\v v' -> v + v' == v' + v)
          :: R3 -> R3 -> Bool)
    ]
    ,testGroup "scalar multiplication"
    [
      testProperty
        "scalar multiplication is distributive in the scalars"
        ((\s s' v -> (s+s').*v == s.*v + s'.*v)
          :: Double -> Double -> R3 -> Bool)
      ,testProperty
        "scalar multiplication is distributive in the vector"
        ((\s v v' -> s .* (v+v') == s.*v + s.*v')
          :: Double -> R3 -> R3 -> Bool)
    ]
    ,testGroup "length and norm of vectors"
    [
      testProperty
        "length of vectors along the axes are just the component"
        ((\(Positive l) -> 
                l ~= vLength (Vec3 (l,0,0)) &&
                l ~= vLength (Vec3 (0,l,0)) &&
                l ~= vLength (Vec3 (0,0,l)))
          :: Positive Double -> Bool)
      ,testProperty
        "norm of a non-zero vector has length 1"
        ((\v -> v /= 0 ==> vLength (vNorm v) ~= 1)
          :: R3 -> Property)
      ,testProperty
        "for non-zero v: len(v) * norm(v) == v"
        ((\v -> v /= 0 ==> vLength v .* vNorm v == v)
          :: R3 -> Property)
    ]

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

-- * instances

instance Arbitrary a => Arbitrary (Vector3 a) where
    arbitrary = do
      [a,b,c] <- vector 3
      return $ Vec3 (a,b,c)
    shrink _ = []