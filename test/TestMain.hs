module Main where

import Test.Tasty (defaultMain, testGroup)

import TestOutParser

main :: IO ()
main = defaultMain $ testGroup "Test all"
  [ TestOutParser.tests
  ]
