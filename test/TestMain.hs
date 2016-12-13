module Main where

import Test.Tasty (defaultMain, testGroup)

import TestOutParser (tests)

main :: IO ()
main = defaultMain $ testGroup "Test all"
  [ TestOutParser.tests
  ]
