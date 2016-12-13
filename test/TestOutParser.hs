{-# LANGUAGE LambdaCase #-}

module TestOutParser where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OutParser

-- | Main to run OutParser tests exclusively.
main :: IO ()
main = defaultMain $ tests

-- | OutParser test group.
tests :: TestTree
tests = testGroup "OutParser tests"
  [ unitTests
  , propTests
  ]

instance Arbitrary Value where
  arbitrary = oneof [B <$> arbitrary, N <$> arbitrary, D <$> arbitrary]
  shrink = \case
    B True -> [B False]
    B False -> []
    N 0 -> []
    N n -> N <$> [n `div` 20, n `div` 2]
    D d -> [D 0]

instance Arbitrary Solution where
  arbitrary = frequency [(5, Sol <$> sol), (1, OptSol <$> sol)]
    where
      sol = take 5 <$> listOf1 var
      var = do
        lbl <- listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ "     ")
        v <- arbitrary
        return (take 35 lbl, v)
  shrink = \case
      Sol vars -> Sol <$> shrink vars
      OptSol vars -> OptSol <$> shrink vars

instance Arbitrary Output where
  arbitrary = Sat . take 5 <$> listOf1 arbitrary
  shrink = \case
    Sat sols -> map Sat $ shrink sols
    Unsat _ -> undefined
    ParseErr _ -> undefined

---- Unit tests ----------------------------------------------------------------

-- | Unit test group
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
    out = unlines
      [ "a : 1"
      , "b : 1"
      , "----------"
      ]
    expected = Sat [Sol [("a", N 1), ("b", N 1)]]
    actual = par out

-- | Test parsing a single optimal result.
unit_single_opt :: Assertion
unit_single_opt = expected @=? actual
  where
    out = unlines
      [ "a : 10"
      , "b : 10"
      , "----------"
      , "=========="
      ]
    expected = Sat [OptSol [("a",N 10),("b",N 10)]]
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
      [ Sol [("a",N 1),("b",N 1)]
      , Sol [("a",N 2),("b",N 2)]
      , Sol [("a",N 3),("b",N 3)]
      ]
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
      [ Sol [("a",N 1), ("b",N 1)]
      , Sol [("a",N 2), ("b",N 2)]
      , OptSol [("a",N 3), ("b",N 3)]
      ]
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
      [ Sol [("a", N 10),("b", N 3),("f", N 4),("g", B True)]
      , Sol [("a", N 10),("b", N 3),("f", N 5),("g", B True)]
      , Sol [("a", N 10),("b", N 3),("f", N 6),("g", B True)]
      , Sol [("a", N 10),("b", N 3),("f", N 7),("g", B True)]
      , Sol [("a", N 10),("b", N 3),("f", N 8),("g", B True)]
      , Sol [("a", N 10),("b", N 3),("f", N 9),("g", B True)]
      , OptSol [("a", N 10),("b", N 3),("f", N 13),("g", B True)]
      ]
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

-- | Property test group
propTests :: TestTree
propTests = testGroup "Property tests"
  [ testProperty "par . show == id" prop_show_par
  ]

-- | par . show == id
prop_show_par :: Output -> Bool
prop_show_par out = par (show out) == out

ex1 = unlines
  [ "rTLn : 7.0"
--  , " Uf : 0"
--  , "YlE : true"
--  , "FA : 2"
  , "----------"
--  , "O : false"
--  , "----------"
  ]

ex2 = unlines
  [ "atHq : 8.440377240699787"
  , "vWbM : true"
  , "----------"
  , "HPR : -3"
  , "W  : -3"
  , "vKb : true"
  , "----------"
  , " Db : -3.122568798608161"
  , "glTj : -2.5730528796468577"
  , "k : 4.8498010947897345"
  , "----------"
  , "=========="
  ]

ex3 = unlines
  [ "a : 1"
  , "a : 1"
  , "----------"
  , "=========="
  ]
