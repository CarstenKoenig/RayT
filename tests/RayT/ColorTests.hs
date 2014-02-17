module RayT.ColorTests 
  ( colorTests
  ) where

import Data.Maybe (isJust)

import Test.HUnit hiding (Test)
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import RayT.Colors

colorTests :: Test
colorTests =
  testGroup "Color tests"
  [
    testGroup "test for equality"
    [
      testCase
        "black is black"
        (True @=? black == black)
      ,testCase
        "black is not white"
        (False @=? black == white)
    ]
  ]