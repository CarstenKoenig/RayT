module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.VectorTests
import RayT.UtilTests
import RayT.PrimitivesTests
import RayT.ColorTests
import RayT.ImageTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ vectorTests
  , utilTests
  , primitivesTests
  , colorTests
  , imageTests
  ]