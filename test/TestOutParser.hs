module TestOutParser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OutParser

-- | Main to run OutParser tests exclusively.
main :: IO ()
main = defaultMain $ tests

tests :: TestTree
tests = testGroup "OutParser tests"
  [ unitTests
  , propTests
  ]

instance Arbitrary Value where
  arbitrary = oneof [B <$> arbitrary, I <$> arbitrary, D <$> arbitrary]

instance Arbitrary Output where
  arbitrary = frequency [(10, sat), (1, unsat), (1, err)]
    where
      sat = Sat <$> listOf1 sol <*> frequency [(1, sol), (5, return [])]
      sol = listOf1 var
      var = do
        lbl <- listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ "   ")
        v <- arbitrary
        return (lbl, v)
      unsat = Unsat <$> arbitrary
      err = ParseErr <$> arbitrary

---- Unit tests ----------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Single non-optimal result" unit_single
  , testCase "Single optimal result" unit_single_opt
  , testCase "Multiple sub-optimal results" unit_multiple
  , testCase "Multiple sub-optimal results + one optimal" unit_multiple_opt
  , testCase "Many results" unit_many
  , testCase "Unsat result" unit_unsat
  ]

-- | Test parsing a single non-optimal result.
unit_single :: Assertion
unit_single = expected @=? actual
  where
    out = "a : 1\nb : 1\n----------"
    expected = Sat [[("a", I 1), ("b", I 1)]] []
    actual = par out

-- | Test parsing a single optimal result.
unit_single_opt :: Assertion
unit_single_opt = expected @=? actual
  where
    out = "a : 10\nb : 10\n----------\n=========="
    expected = Sat [[("a",I 10),("b",I 10)]] [("a",I 10),("b",I 10)]
    actual = par out

-- | Test parsing multiple sub-optimal results.
unit_multiple :: Assertion
unit_multiple = expected @=? actual
  where
    out = unlines
      [ "a : 1"
      , "b : 1"
      , "----------"
      , "a : 2"
      , "b : 2"
      , "----------"
      , "a : 3"
      , "b : 3"
      , "----------"
      ]
    expected = Sat
      [ [("a",I 1),("b",I 1)]
      , [("a",I 2),("b",I 2)]
      , [("a",I 3),("b",I 3)]
      ]
      []
    actual = par out

-- | Test parsing of multiple non-optimal results ending with an optimal result.
unit_multiple_opt :: Assertion
unit_multiple_opt = expected @=? actual
  where
    out = unlines
      [ "a : 1"
      , "b : 1"
      , "----------"
      , "a : 2"
      , "b : 2"
      , "----------"
      , "a : 3"
      , "b : 3"
      , "----------"
      , "=========="
      ]
    expected = Sat
      [ [("a",I 1), ("b",I 1)]
      , [("a",I 2), ("b",I 2)]
      , [("a",I 3), ("b",I 3)]
      ]
      [("a",I 3), ("b",I 3)]
    actual = par out

-- | Test parsing of many results.
unit_many = expected @=? actual
  where
    out = unlines
      [ "a : 10\nb : 3\nf : 4\ng : true\n----------"
      , "a : 10\nb : 3\nf : 5\ng : true\n----------"
      , "a : 10\nb : 3\nf : 6\ng : true\n----------"
      , "a : 10\nb : 3\nf : 7\ng : true\n----------"
      , "a : 10\nb : 3\nf : 8\ng : true\n----------"
      , "a : 10\nb : 3\nf : 9\ng : true\n----------"
      , "a : 10\nb : 3\nf : 13\ng : true\n----------\n=========="
      ]
    expected = Sat
      [ [("a", I 10),("b", I 3),("f", I 4),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 5),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 6),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 7),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 8),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 9),("g", B True)]
      , [("a", I 10),("b", I 3),("f", I 13),("g", B True)]
      ]
      [("a",I 10),("b",I 3),("f",I 13),("g",B True)]
    actual = par out

-- | Test parsing of unsat result.
unit_unsat = expected @=? actual
  where
    out = unlines
      [ ""
      , "WARNING: model inconsistency detected"
      , "test.mzn:4:"
      , "in binary '>' operator expression"
      , "=====UNSATISFIABLE====="
      , "% test.fzn:1: warning: model inconsistency detected before search."
      ]
    expected = Unsat out
    actual = par out

---- Property tests ------------------------------------------------------------

propTests :: TestTree
propTests = testGroup "Property tests"
  [ testProperty "par . show == id" prop_show_par
  ]

prop_show_par :: Output -> Bool
prop_show_par out = par (show out) == out
