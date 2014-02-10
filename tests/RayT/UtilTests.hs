module RayT.UtilTests 
  ( utilTests
  ) where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.Utils
import RayT.Vector

utilTests :: Test
utilTests =
  testGroup "Utility tests"
  [
    testGroup "solveQ tests"
    [
     testProperty 
        "can solve for given solutions"
        ((\a b c -> solves a b c $ solveQ a b c)
          :: Double -> Double -> Double -> Bool)
      ,testCase
        "solves simple equation"
        ([2, 1] @=? solveQ 4 (-12) 8)
    ]
  ]

solves :: Double -> Double -> Double -> [Double] -> Bool
solves a b c []       = True
solves a b c (s:sols) = a*s*s + b*s + c ~= 0 && solves a b c sols