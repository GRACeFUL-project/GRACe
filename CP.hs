{-# LANGUAGE GADTs #-}
module CP ((.<),
           (.<=),
           (===),
           nt,
           lit,
           (.&&),
           max',
           min',
           CP,
           CPExp,
           assert,
           value,
           CPType,
           inRange
          ) where
import Port
import Program

-- Things that are supported by the CP runtime
class CPType a
instance CPType Int
instance CPType Float
instance CPType Bool

-- Constraint program expressions
data CPExp a where
    ValueOf :: (CPType a)        => Port a     -> CPExp a
    Lit     :: (CPType a)        => a          -> CPExp a
    Equal   :: (CPType a, Eq a)  => CPExp a    -> CPExp a    -> CPExp Bool
    LeThan  :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp Bool
    LtEq    :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp Bool
    Add     :: (CPType a, Num a) => CPExp a    -> CPExp a    -> CPExp a
    Mul     :: (CPType a, Num a) => CPExp a    -> CPExp a    -> CPExp a 
    Sub     :: (CPType a, Num a) => CPExp a    -> CPExp a    -> CPExp a 
    Max     :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp a
    Min     :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp a
    Not     ::                      CPExp Bool -> CPExp Bool
    And     ::                      CPExp Bool -> CPExp Bool -> CPExp Bool

-- Constraint program commands
data CPCommands a where
    Assert  ::               CPExp Bool -> CPCommands ()

-- Syntactic sugar for expressions
instance (Num a, CPType a) => Num (CPExp a) where
    (+) = Add
    (*) = Mul
    (-) = Sub
    negate x = (lit 0) - x
    abs = undefined
    signum = undefined
    fromInteger = lit . fromInteger

(.<) :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp Bool
(.<) = LeThan

(.<=) :: (CPType a, Ord a) => CPExp a    -> CPExp a    -> CPExp Bool
(.<=) = LtEq

(===) :: (CPType a, Eq a) => CPExp a    -> CPExp a    -> CPExp Bool
(===) = Equal

infixr 0 ===

nt :: CPExp Bool -> CPExp Bool
nt  = Not

lit :: (CPType a) => a -> CPExp a
lit  = Lit

(.&&) :: CPExp Bool -> CPExp Bool -> CPExp Bool
(.&&) = And

max' :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp a
max' = Max

min' :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp a
min' = Min

-- Constraint programs
type CP a = Program CPCommands a

-- Syntactic sugar for expressions in the CP "monad"
assert :: CPExp Bool -> CP ()
assert bexp = Instr (Assert bexp)

-- Unsure about if this is the best programming model for this
value  :: (CPType a) => Port a -> CP (CPExp a)
value p = return (ValueOf p)

-- Some derived operators
inRange :: (CPType a, Ord a) => CPExp a -> (CPExp a, CPExp a) -> CPExp Bool
inRange a (low, high) = (low .<= a) .&& (a .<= high)
