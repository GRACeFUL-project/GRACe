{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}

module CP where

import Control.Monad.Writer hiding (Sum)
import Control.Monad.State.Lazy
import Data.Char
import Program
import System.Process

-- * Constraint Programming
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Proxy a = Proxy

data Variable a = Var { varID :: Int } deriving (Eq, Functor)

instance Show (Variable a) where
  show (Var n) = show n

-- | Types supported by the constraint programming runtime.
class (Show a, Eq a) => CPType a where
  typeDec :: Proxy a -> String -> String

-- We have to constraint integers to this range, because the solver is kind of dumb
defaultIntRange :: String
defaultIntRange = " -10000000..10000000"

instance CPType Int where
  typeDec = const (++ defaultIntRange)

instance CPType Float where
  typeDec = const (++ " float")

instance CPType Bool where
  typeDec = const (++ " bool")

-- | Expressions in the Constraint Programming monad.
data CPExp a where
  -- | This will require extra documentation.
  ValueOf ::                             Variable a -> CPExp a
  Lit     :: CPType a                 => a          -> CPExp a
  Equal   :: CPType a                 => CPExp a    -> CPExp a    -> CPExp Bool
  LeThan  :: (CPType a, Ord a)        => CPExp a    -> CPExp a    -> CPExp Bool
  LtEq    :: (CPType a, Ord a)        => CPExp a    -> CPExp a    -> CPExp Bool
  Add     :: (CPType a, Num a)        => CPExp a    -> CPExp a    -> CPExp a
  Mul     :: (CPType a, Num a)        => CPExp a    -> CPExp a    -> CPExp a
  Sub     :: (CPType a, Num a)        => CPExp a    -> CPExp a    -> CPExp a
  Max     :: (CPType a, Ord a)        => CPExp a    -> CPExp a    -> CPExp a
  Min     :: (CPType a, Ord a)        => CPExp a    -> CPExp a    -> CPExp a
  Not     ::                             CPExp Bool -> CPExp Bool
  And     ::                             CPExp Bool -> CPExp Bool -> CPExp Bool
  Div     :: (Fractional a, CPType a) => CPExp a    -> CPExp a -> CPExp a
  I2F     ::                             CPExp Int  -> CPExp Float
  ForAll  ::                             ComprehensionMonad (CPExp Bool) -> CPExp Bool
  Sum     :: (Num a)                  => ComprehensionMonad (CPExp a)    -> CPExp a
  MaxA    :: (Ord a)                  => ComprehensionMonad (CPExp a)    -> CPExp a
  MinA    :: (Ord a)                  => ComprehensionMonad (CPExp a)    -> CPExp a
  -- This makes me a sad sad boy :(
  {-IdxA1D  :: CPType a                 => CPExp (Array1D a) -> CPExp Int   -> CPExp a-}
  {-IdxA2D  :: CPType a                 => CPExp (Array2D a) -> (CPExp Int, CPExp Int) -> CPExp a-}

-- | An array and how it is indexed
{-class IsArray a idx | a -> idx where-}
  {-type CPDomain idx :: *-}
  {-indexArray  :: CPType x => CPExp (a x) -> CPDomain idx -> CPExp x-}
  {-createArray :: CPType x => idx -> GCM (Variable (a x))-}

-- | A 1D array is an Name and a size
{-data Array1D x-}

-- | A 2D array is an Name and a size
{-data Array2D x-}

-- | How to construct and access a 1D array
{-instance IsArray Array1D Int where-}
  {-type CPDomain Int = CPExp Int -}
  {-indexArray  = IdxA1D -}
  {-createArray = Instr . (CreateArray1D Proxy)-}

-- | How to construct and access a 2D array
{-instance IsArray Array2D (Int, Int) where-}
  {-type CPDomain (Int, Int) = (CPExp Int, CPExp Int)-}
  {-indexArray  = IdxA2D-}
  {-createArray = Instr . (CreateArray2D Proxy)-}

-- | Commands for things from which we can construct "forall ..." expressions
data ComprehensionCommand a where
  Range :: (CPType a, Ord a) => (CPExp a, CPExp a) -> ComprehensionCommand (CPExp a)

-- | A monad representing what goes on in a "forall/sum/max/min ..." block
type ComprehensionMonad a = Program ComprehensionCommand a

(...) :: (CPType a, Ord a) => CPExp a -> CPExp a -> ComprehensionMonad (CPExp a)
x ... y = Instr $ Range (x, y)

-- Not well defined (CPType much too general).
instance (Num a, CPType a) => Num (CPExp a) where
  (+)         = Add
  (*)         = Mul
  (-)         = Sub
  abs         = undefined
  signum      = undefined
  fromInteger = lit . fromInteger

-- Not well defined (CPType much too general).
instance (Fractional a, CPType a) => Fractional (CPExp a) where
  (/)          = Div
  fromRational = lit . fromRational

-- * The CP monad
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Instructions in the constraint programming monad.
data CPCommands a where
  Assert        ::             CPExp Bool -> CPCommands ()
  CreateLVar    :: CPType a => Proxy a    -> CPCommands (Variable a)


-- | Constraint programs.
type CP a = Program CPCommands a

-- | Assert expressions in the `CP` monad.
assert :: CPExp Bool -> CP ()
assert = Instr . Assert

-- | Create a new local variable in the `CP` monad.
createLVar :: CPType a => CP (Variable a)
createLVar = Instr $ CreateLVar Proxy

-- * Base CP operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(.<) :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp Bool
(.<) = LeThan

(.<=) :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp Bool
(.<=) = LtEq

(===) :: (CPType a) => CPExp a -> CPExp a -> CPExp Bool
(===) = Equal

(.&&) :: CPExp Bool -> CPExp Bool -> CPExp Bool
(.&&) = And

{-(.!!) :: (CPType x, IsArray a idx) => CPExp (a x) -> CPDomain idx -> CPExp x-}
{-(.!!) = indexArray-}

nt :: CPExp Bool -> CPExp Bool
nt  = Not

lit :: (CPType a) => a -> CPExp a
lit  = Lit

max' :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp a
max' = Max

min' :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp a
min' = Min

i2f :: CPExp Int -> CPExp Float
i2f = I2F

forAll :: ComprehensionMonad (CPExp Bool) -> CPExp Bool
forAll = ForAll

summ :: (Num a) => ComprehensionMonad (CPExp a) -> CPExp a
summ = Sum

maximumm, minimumm :: (Ord a) => ComprehensionMonad (CPExp a) -> CPExp a
maximumm = MaxA
minimumm = MinA

-- | Unsure about if this is the best programming model for this
val  :: Variable a -> CPExp a
val = ValueOf

infixr 3 .&&
infixl 4 .<
infixl 4 .<=
infix  4 ===

-- * Derived CP operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(.||) :: CPExp Bool -> CPExp Bool -> CPExp Bool
a .|| b = nt (nt a .&& nt b)

(==>) :: CPExp Bool -> CPExp Bool -> CPExp Bool
a ==> b = (nt a) .|| b

(.>) :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp Bool
a .> b = b .< a

(.>=) :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp Bool
a .>= b = b .<= a

(/==)  :: (CPType a, Ord a) => CPExp a -> CPExp a -> CPExp Bool
a /== b = nt (a === b)

-- | @x `inRange` (l, r)@ evaluates to @True@ if @x@ is on the closed interval
-- @[l, r]@.
inRange :: (CPType a, Ord a) => CPExp a -> (CPExp a, CPExp a) -> CPExp Bool
inRange a (low, high) = (low .<= a) .&& (a .<= high)

infixr 2 .||
infixl 4 .>
infixl 4 .>=
