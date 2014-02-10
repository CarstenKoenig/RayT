module RayT.UnitDoubleTests 
  ( unitDoubleTests
  ) where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.Vector ((~=))
import RayT.UnitDouble

unitDoubleTests :: Test
unitDoubleTests =
  testGroup "unit double tests"
  [
    testGroup "addition and multiplication"
    [
     testProperty 
        "0 is the additive neutral element"
        ((\f -> f + 0 == f && 0 + f == f)
          :: UnitDouble -> Bool)
     ,testProperty 
        "1 is the multiplicative neutral element"
        ((\f -> f * 1 == f && 1 * f == f)
          :: UnitDouble -> Bool)
     ,testProperty 
        "addition is bounded at 1"
        ((\f g -> valueD (f+g) <= 1)
          :: UnitDouble -> UnitDouble -> Bool)
     ,testProperty 
        "multiplication is bounded at 1"
        ((\f g -> valueD (f*g) <= 1)
          :: UnitDouble -> UnitDouble -> Bool)
     ,testProperty 
        "addition is bounded at 0"
        ((\f g -> valueD (f+g) >= 0)
          :: UnitDouble -> UnitDouble -> Bool)
     ,testProperty 
        "multiplication is bounded at 0"
        ((\f g -> valueD (f*g) >= 0)
          :: UnitDouble -> UnitDouble -> Bool)
    ]
  ]

-- * instances

instance Arbitrary UnitDouble where
    arbitrary = do
      d <- arbitrary
      return $ unitD d
    shrink _ = []