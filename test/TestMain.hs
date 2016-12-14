module Main where

import Test.Tasty (defaultMain, testGroup)

import TestOutParser (tests)
import TestFW.TestGCMCheck (tests)

main :: IO ()
main = defaultMain $ testGroup "Test all"
  [ TestOutParser.tests
  , TestFW.TestGCMCheck.tests
  ]
