{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Sign
  ( Sign(..)
  , tSign
  ) where

import CP
import Types
import Utils
import qualified Interfaces.MZASTBase as HZ

-- * A Sign type to define and reason about CLDs
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Sign = M   -- minus
          | Z   -- zero
          | P   -- plus
          | Q   -- ambiguous
          deriving (Eq, Ord)  -- ordering of constructors is important for Ord

instance CPType Sign where
  typeDec = const (++ " -2..2")
  hzType  = const (HZ.Range (HZ.IConst $ -2) (HZ.IConst 2))
  hzConst = HZ.IConst . to iso

instance Num Sign where
  x + y = case (x, y) of
    (Z, b) -> b
    (a, Z) -> a
    (P, P) -> P
    (M, M) -> M
    (P, M) -> Q
    (M, P) -> Q
    (Q, _) -> Q
    (_, Q) -> Q
  fromInteger = from iso . fromInteger

instance Show Sign where
  show x = case x of
    Z -> "0"
    P -> "1"
    M -> "-1"
    Q -> "2"

tSign :: Type Sign
tSign = Iso iso tInt

instance IsTyped Sign where
    typeOf _ = tSign
    fromTyped (x ::: t) = equalM t tSign >>= \f -> return (f x)

iso :: Isomorphism Int Sign
iso = f <-> g
 where
  f :: Int -> Sign
  f x | x == -1   = M
      | x ==  0   = Z
      | x ==  1   = P
      | otherwise = Q

  g :: Sign -> Int
  g s = case s of
    M -> -1
    Z -> 0
    P -> 1
    Q -> 2
