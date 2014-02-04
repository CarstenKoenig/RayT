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
  	testGroup "stupid Tests"
    [
     testCase 
        "einfache HUNIT"
        simpleAssert
     ,testProperty 
        "einfacher Check"
        simpleCheck
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