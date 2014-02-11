module RayT.PrimitivesTests 
  ( primitivesTests
  ) where

import Data.Maybe (isJust)

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT
import RayT.Primitives
import RayT.Vector
import RayT.Utils
import RayT.Colors

primitivesTests :: Test
primitivesTests =
  testGroup "Primitives tests"
  [
    testGroup "testing the sphere"
    [
     testCase 
        "ray straight along the Z-axis hits a default sphere"
        (True @=? doesIntersect (ray 0 bZ) defaultSphere)
     ,testCase 
        "ray above the sphere will not hit"
        (False @=? doesIntersect (ray 0 (10.*bZ + 1.1.*bY)) defaultSphere)
     ,testCase 
        "trace-ray straight along the Z-axis hits a default sphere"
        (True @=? white == traceRay [defaultSphere] (ray 0 bZ))
     ,testCase 
        "trace-ray above the sphere will not hit"
        (True @=? black == traceRay [defaultSphere] (ray 0 (10.*bZ + 1.1.*bY)))
    ]
  ]

defaultSphere :: Object
defaultSphere = sphere (Mat white) (10.*bZ) 1

doesIntersect :: Ray -> Object -> Bool
doesIntersect r o = isJust $ o r