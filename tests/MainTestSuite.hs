module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.VectorTests
import RayT.UnitDoubleTests
import RayT.UtilTests
import RayT.PrimitivesTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ vectorTests
  , unitDoubleTests
  , utilTests
  , primitivesTests
  ]