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
           inRange,
           Proxy(..),
           translateCPCommands,
           typeDec
          ) where
import Port
import Program
import Control.Monad.Writer

data Proxy a = Proxy

-- Things that are supported by the CP runtime
class (Show a) => CPType a where
    typeDec :: Proxy a -> String

instance CPType Int where
    typeDec = const "int"

instance CPType Float where
    typeDec = const "float"

instance CPType Bool where
    typeDec = const "bool"

-- Constraint program expressions
data CPExp a where
    ValueOf :: (CPType a)         => Port a     -> CPExp a
    Lit     :: (CPType a, Show a) => a          -> CPExp a
    Equal   :: (CPType a, Eq a)   => CPExp a    -> CPExp a    -> CPExp Bool
    LeThan  :: (CPType a, Ord a)  => CPExp a    -> CPExp a    -> CPExp Bool
    LtEq    :: (CPType a, Ord a)  => CPExp a    -> CPExp a    -> CPExp Bool
    Add     :: (CPType a, Num a)  => CPExp a    -> CPExp a    -> CPExp a
    Mul     :: (CPType a, Num a)  => CPExp a    -> CPExp a    -> CPExp a
    Sub     :: (CPType a, Num a)  => CPExp a    -> CPExp a    -> CPExp a
    Max     :: (CPType a, Ord a)  => CPExp a    -> CPExp a    -> CPExp a
    Min     :: (CPType a, Ord a)  => CPExp a    -> CPExp a    -> CPExp a
    Not     ::                       CPExp Bool -> CPExp Bool
    And     ::                       CPExp Bool -> CPExp Bool -> CPExp Bool

compileCPExp :: (CPType a) => CPExp a -> String
compileCPExp (ValueOf (Port i)) = "v"++ (show i)
compileCPExp (Lit l)            = show l
compileCPExp (Equal a b)        = comp2paren a " == " b
compileCPExp (LeThan a b)       = comp2paren a " < "  b
compileCPExp (LtEq a b)         = comp2paren a " <= " b
compileCPExp (Add a  b)         = comp2paren a " + "  b
compileCPExp (Mul a  b)         = comp2paren a " * "  b
compileCPExp (Sub a  b)         = comp2paren a " - "  b
compileCPExp (Max a  b)         = "max" ++ paren (comp2paren a "," b)
compileCPExp (Min a  b)         = "min" ++ paren (comp2paren a "," b)
compileCPExp (And a  b)         = comp2paren a " /\\ "b
compileCPExp (Not a)            = "not " ++ paren (compileCPExp a)

comp2paren :: (CPType a, CPType b) => CPExp a -> String -> CPExp b -> String
comp2paren a op b = paren (compileCPExp a) ++ op ++ paren (compileCPExp b)

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- Constraint program commands
data CPCommands a where
    Assert :: CPExp Bool -> CPCommands ()

translateCPCommands :: CPCommands a -> Writer [String] a
translateCPCommands (Assert bexp) = tell (["constraint " ++ paren (compileCPExp bexp)++";"])

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

infix 4 ===

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
