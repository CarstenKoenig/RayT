module RayT.ImageTests 
  ( imageTests
  ) where

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.Vector
import RayT
import RayT.Image

imageTests :: Test
imageTests =
  testGroup "Image module tests"
  [
    testGroup "rasterization tests"
    [
      testCase
        "center pixel in a 3x3 image is projected to 0 using a (6x6) default-screen"
        (0 @=? rasterPoint (defaultScreen (6,6)) (3,3) (1,1))
      ,testCase
        "top-left pixel in a 3x3 image is projected to (-2,2,0) using a (6x6) default-screen"
        (Vec3 (-2,2,0) @=? rasterPoint (defaultScreen (6,6)) (3,3) (0,0))
      ,testCase
        "bottom-right pixel in a 3x3 image is projected to (2,-2,0) using a (6x6) default-screen"
        (Vec3 (2,-2,0) @=? rasterPoint (defaultScreen (6,6)) (3,3) (2,2))
      ,testCase
        "center pixel in a 3x3 image yields a ray along the Z-axis"
        (target 0 @=? rasterRay (defaultCamera (-10) (6,6)) (3,3) (1,1))
    ]
  ]

target :: R3 -> Ray
target = rayTo cam
  where cam = (-10).*bZ
