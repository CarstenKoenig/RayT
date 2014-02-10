module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.VectorTests
import RayT.UnitDoubleTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ vectorTests
  , unitDoubleTests
  ]