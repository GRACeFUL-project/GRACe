{-# LANGUAGE GADTs #-}
module CP ((.+),
           (.*),
           (.<),
           (===),
           nt,
           lit,
           (.&&),
           CP,
           CPExp,
           assert,
           value
          ) where
import Port
import Program

-- Names of variables
type Name = Int

-- Things that support equality
class Eq' a
instance Eq' Int
instance Eq' Bool

-- Things that support Ordering
class Ord' a
instance Ord' Int

-- Things that support numeric expressions
class Num' a
instance Num' Int

-- Things that are supported by the CP runtime
class CPType a
instance CPType Int
instance CPType Bool

-- Constraint program expressions
data CPExp a where
    ValueOf :: (CPType a)         => Port a     -> CPExp a
    Lit     :: (CPType a)         => a          -> CPExp a
    Equal   :: (CPType a, Eq' a)  => CPExp a    -> CPExp a    -> CPExp Bool
    LeThan  :: (CPType a, Ord' a) => CPExp a    -> CPExp a    -> CPExp Bool
    Add     :: (CPType a, Num' a) => CPExp a    -> CPExp a    -> CPExp a
    Mul     :: (CPType a, Num' a) => CPExp a    -> CPExp a    -> CPExp a 
    Not     ::                       CPExp Bool -> CPExp Bool
    And     ::                       CPExp Bool -> CPExp Bool -> CPExp Bool

-- Constraint program commands
data CPCommands a where
    Assert  ::               CPExp Bool -> CPCommands ()

-- Syntactic sugar for expressions
(.+) :: (CPType a, Num' a) => CPExp a    -> CPExp a    -> CPExp a 
(.+)  = Add

(.*) :: (CPType a, Num' a) => CPExp a    -> CPExp a    -> CPExp a
(.*)  = Mul

(.<) :: (CPType a, Ord' a) => CPExp a    -> CPExp a    -> CPExp Bool
(.<)  = LeThan

(===) :: (CPType a, Eq' a) => CPExp a    -> CPExp a    -> CPExp Bool
(===) = Equal

nt :: CPExp Bool -> CPExp Bool
nt  = Not

lit :: (CPType a) => a -> CPExp a
lit  = Lit

(.&&) :: CPExp Bool -> CPExp Bool -> CPExp Bool
(.&&) = And

-- Constraint programs
type CP a = Program CPCommands a

-- Syntactic sugar for expressions in the CP "monad"
assert :: CPExp Bool -> CP ()
assert bexp = Instr (Assert bexp)

-- Unsure about if this is the best programming model for this
value  :: (CPType a) => Port a -> CP (CPExp a)
value p = return (ValueOf p)
