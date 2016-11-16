{-# LANGUAGE GADTs #-}
module CP where

-- Names of variables
type Name = Int

-- Things that support equality
class Eq' a
instance Eq' Int
instance Eq' Bool

-- Things that support Ordering
class Ord' a
instance Ord' Int

-- Things that are supported by the CP runtime
class CPType a
instance CPType Int
instance CPType Bool

-- Constraint program expressions
data CPExp a where
    Var    :: (CPType a)         => Name       -> CPExp a
    Lit    :: (CPType a)         => a          -> CPExp a
    Equal  :: (CPType a, Eq' a)  => CPExp a    -> CPExp a -> CPExp Bool
    LeThan :: (CPType a, Ord' a) => CPExp a    -> CPExp a -> CPExp Bool
    Ife    :: (CPType a)         => CPExp Bool -> CPExp a -> CPExp a    -> CPExp a
    Assert ::                       CPExp Bool -> CPExp ()
