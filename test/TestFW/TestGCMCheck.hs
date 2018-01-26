module TestFW.TestGCMCheck where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import TestFW.GCMCheck

import Data.Map (fromList)

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
    sol = fromList [("mu", False), ("groda", True)]
    expected = Nothing
    actual = verify sol

-- | Test solution with one @"prop_"@erty that passed.
unit_one_prop_passed :: Assertion
unit_one_prop_passed = expected @=? actual
  where
    sol = fromList [("mu", False), ("prop_gurka", True), ("groda", False)]
    expected = Nothing
    actual = verify sol

-- | Test solution with one @"prop_"@erty that failed.
unit_one_prop_failed :: Assertion
unit_one_prop_failed = expected @=? actual
  where
    sol = fromList [ ("mu", False)
              , ("smu", True)
              , ("prop_gurka", False)
              , ("groda", False)
              ]
    expected = Just sol
    actual = verify sol

-- | Test solution with two @"prop_"@erties that passed.
unit_two_prop_passed :: Assertion
unit_two_prop_passed = expected @=? actual
  where
    sol = fromList [ ("mu", False)
              , ("prop_gurka", True)
              , ("groda", False)
              , ("prop_prop", True)
              ]
    expected = Nothing
    actual = verify sol

-- | Test solution with two @"prop_"@erties, one failed.
unit_two_prop_one_failed :: Assertion
unit_two_prop_one_failed = expected @=? actual
  where
    sol = fromList [ ("mu", False)
              , ("prop_gurka", True)
              , ("groda", True)
              , ("prop_prop", False)
              ]
    expected = Just sol
    actual = verify sol

-- | Test solution with two @"prop_"@erties, both failed.
unit_two_prop_two_failed :: Assertion
unit_two_prop_two_failed = expected @=? actual
  where
    sol = fromList [ ("mu", False)
              , ("prop_gurka", False)
              , ("groda", True)
              , ("prop_prop", False)
              ]
    expected = Just sol
    actual = verify sol
