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
import RayT.Lights

primitivesTests :: Test
primitivesTests =
  testGroup "Primitives tests"
  [
    testGroup "testing the sphere"
    [
     testCase 
        "ray straight along the Z-axis hits a default sphere"
        (True @=? doesIntersect (target 0) sampleSphere)
     ,testCase 
        "ray above the sphere will not hit"
        (False @=? doesIntersect (target (10.*bZ + 1.1.*bY)) sampleSphere)
     ,testCase 
        "trace-ray straight along the Z-axis hits a default sphere"
        (True @=? black /= trace (target 0))
     ,testCase 
        "trace-ray above the sphere will not hit"
        (True @=? black == trace (target (10.*bZ + 1.1.*bY)))
    ]
  ]

trace :: Ray -> Color
trace = traceRay camPos sampleScene

target :: R3 -> Ray
target = rayTo camPos

camPos :: R3
camPos = (-10).*bZ

sampleScene :: Scene
sampleScene = Scene [sampleSphere] [AmbientLight white]

sampleSphere :: Object
sampleSphere = sphere (Mat white) (10.*bZ) 1

doesIntersect :: Ray -> Object -> Bool
doesIntersect r o = isJust $ o r