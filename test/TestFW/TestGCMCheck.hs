module TestFW.TestGCMCheck where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OutParser
import TestFW.GCMCheck

-- | Main to run GCMCheck tests exclusively.
main :: IO ()
main = defaultMain tests

-- | GCMCheck test group.
tests :: TestTree
tests = testGroup "GCMCheck tests"
  [ unitTests
  ]

unitTests = testGroup "Unit tests"
   [ testCase "No property in solution" unit_no_prop
   , testCase "One property in solution which passed" unit_one_prop_passed
   , testCase "One property in solution which failed" unit_one_prop_failed
   , testCase "Two properties in solution which passed" unit_two_prop_passed
   , testCase "Two properties in solution, one failed" unit_two_prop_one_failed
   , testCase "Two properties in solution, both failed" unit_two_prop_two_failed
   ]

-- | Test no variable with the prefix @"prop_"@.
unit_no_prop :: Assertion
unit_no_prop = expected @=? actual
  where
    sol = Sol [("mu", B False), ("groda", D 0.005)] True
    expected = Nothing
    actual = verify sol

-- | Test solution with one @"prop_"@erty that passed.
unit_one_prop_passed :: Assertion
unit_one_prop_passed = expected @=? actual
  where
    sol = Sol [("mu", B False), ("prop_gurka", B True), ("groda", D 0.005)] True
    expected = Nothing
    actual = verify sol

-- | Test solution with one @"prop_"@erty that failed.
unit_one_prop_failed :: Assertion
unit_one_prop_failed = expected @=? actual
  where
    sol = Sol [ ("mu", B False)
              , ("smu", N 42)
              , ("prop_gurka", B False)
              , ("groda", D 0.005)
              ]
              True
    expected = Just sol
    actual = verify sol

-- | Test solution with two @"prop_"@erties that passed.
unit_two_prop_passed :: Assertion
unit_two_prop_passed = expected @=? actual
  where
    sol = Sol [ ("mu", B False)
              , ("prop_gurka", B True)
              , ("groda", D 0.005)
              , ("prop_prop", B True)
              ]
              False
    expected = Nothing
    actual = verify sol

-- | Test solution with two @"prop_"@erties, one failed.
unit_two_prop_one_failed :: Assertion
unit_two_prop_one_failed = expected @=? actual
  where
    sol = Sol [ ("mu", B False)
              , ("prop_gurka", B True)
              , ("groda", D 0.005)
              , ("prop_prop", B False)
              ]
              False
    expected = Just sol
    actual = verify sol

-- | Test solution with two @"prop_"@erties, both failed.
unit_two_prop_two_failed :: Assertion
unit_two_prop_two_failed = expected @=? actual
  where
    sol = Sol [ ("mu", B False)
              , ("prop_gurka", B False)
              , ("groda", D 0.005)
              , ("prop_prop", B False)
              ]
              False
    expected = Just sol
    actual = verify sol
