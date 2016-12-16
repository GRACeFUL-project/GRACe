{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-} 
{-# LANGUAGE UndecidableInstances #-}

module GL where

import Control.Monad.Writer
import Control.Monad.State.Lazy
import Data.Char
import Program
import System.Process

-- * Constraint Programming
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Proxy a = Proxy

-- | Base types supported by the constraint programming runtime.
class (Show a, Eq a) => CPBaseType a where
    typeDecBase :: Proxy a -> String -> String

-- | Types supported by the constraint programming runtime.
class (Show a, Eq a) => CPType a where
    typeDec :: Proxy a -> String -> String

instance CPBaseType Int where 
    -- We have to constraint integers to this range, because the solver is kind of dumb
    typeDecBase = const (++ " -10000000..10000000")

instance CPBaseType Float where
    typeDecBase = const (++ " float")

instance CPBaseType Bool where
    typeDecBase = const (++ " bool")

instance (CPBaseType a, Show a, Eq a) => CPType a where
    typeDec = typeDecBase

-- | Expressions in the Constraint Programming monad.
data CPExp a where
    -- | This will require extra documentation.
    ValueOf :: (CPType a, IsPort p)     => p a        -> CPExp a
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
    Div     :: (Fractional a, CPType a) => CPExp a -> CPExp a -> CPExp a
    I2F     :: CPExp Int -> CPExp Float

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
    Assert :: CPExp Bool -> CPCommands ()

-- | Constraint programs.
type CP a = Program CPCommands a

-- | Expressions in the `CP` monad.
assert :: CPExp Bool -> CP ()
assert = Instr . Assert 

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

-- | Unsure about if this is the best programming model for this
value  :: (CPType a, IsPort p) => p a -> CP (CPExp a)
value = return . ValueOf

val  :: (CPType a, IsPort p) => p a -> CPExp a
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

-- * The GCM monad
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | GRACeFUL Concept Map commands.
data GCMCommand a where
    Output       :: CPType a => Port a    -> String -> GCMCommand ()
    CreatePort   :: CPType a => Proxy a   -> GCMCommand (Port a)
    -- | Unsure that hardcoding this to Int is a good idea
    CreateGoal   ::             GCMCommand (Goal Int) 
    CreateParam  :: CPType a => Proxy a   -> a -> GCMCommand (Param a)
    CreateAction :: CPType a => Param a   -> GCMCommand (Action a)
    EmbedAction  ::             ActM a    -> GCMCommand ()
    Component    ::             CP ()     -> GCMCommand ()

-- | A GRACeFUL Concept Map.
type GCM = Program GCMCommand

-- | Action placeholder.
data Action a = Action Int (Param a)

data ActCommand a where
    Act :: CPType a => CPExp a -> Action a -> ActCommand ()

type ActM = Program ActCommand

-- * Base GCM operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @'output' p str@ displays the value at @p@ with label @str@ during solver
-- output.
output :: (CPType a, Show a) => Port a -> String -> GCM ()
output p = Instr . Output p

-- | Instantiate a action in the `GCM` monad.
createAction :: CPType a => (CPExp Int -> CPExp a -> CPExp a) -> Param a -> GCM (Action a)
createAction foo p = do
  a <- Instr (CreateAction p)
  action $ act foo a
  return a

-- | Instantiate a port in the `GCM` monad.
createPort :: CPType a => GCM (Port a)
createPort = Instr (CreatePort Proxy)

-- | Instantiate a goal in the `GCM` monad.
createGoal :: GCM (Goal Int)
createGoal = Instr CreateGoal

-- | Instantiate a parameter in the `GCM` monad.
createParam :: CPType a => a -> GCM (Param a)
createParam = Instr . CreateParam Proxy

-- | Run actions in the `GCM` monad.
action :: ActM a -> GCM ()
action = Instr . EmbedAction

-- | @'act' f a@ applies the function @f@ to the action @a@.
act :: CPType a => (CPExp Int -> CPExp a -> CPExp a) -> Action a -> ActM ()
act f act@(Action i (Param a j)) = Instr (Act (f (ValueOf (Port i)) (lit a)) act)

-- | Document this.
component :: CP () -> GCM ()
component = Instr . Component

-- * Derived GCM operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- See if the action was taken.
taken :: CPType a => Action a -> GCM (Port Int)
taken (Action i _) = return (Port i)

-- | @'link' p1 p2@ creates a connection from port @p1@ to port @p2@.
link :: CPType a => Port a -> Port a -> GCM ()
link p1 p2 = component $ do
    v1 <- value p1
    v2 <- value p2
    assert $ v1 === v2

-- | @'mutex' a1 a2@ ensures that actions @a1@ and @a2@ are mutually exclusive.
mutex :: (CPType a, CPType b) => Action a -> Action b -> GCM ()
mutex a1 a2 = do
    p1 <- taken a1
    p2 <- taken a2
    component $ do
        v1 <- value p1
        v2 <- value p2
        assert $ nt ((v1 .> 0) .&& (v2 .> 0))

-- | @'fun' f a b@ documentation goes here.
fun :: (CPType a, CPType b, IsPort pa, IsPort pb) 
    => (CPExp a -> CPExp b) 
    -> pa a 
    -> pb b 
    -> GCM ()
fun f a b = component $ do
    i <- value a
    o <- value b
    assert $ o === f i

set :: (CPType a) => Port a -> a -> GCM ()
set p a = component $ do
    v <- value p
    assert $ v === lit a

-- | TODO: Nice, but unused.
foldGCM :: (CPType a, CPType b) 
        => Int 
        -> (CPExp b -> CPExp a -> CPExp b) 
        -> CPExp b 
        -> GCM ([Port a], Port b)
foldGCM i f v = do
    inputs <- replicateM i createPort
    output <- createPort
    component $ do
        values <- mapM value inputs
        outv   <- value output
        assert $ outv === foldl f v values
    return (inputs, output)

-- | TODO: Nice, but unused.
sumGCM :: (CPType a, Num a) => Int -> GCM ([Port a], Port a)
sumGCM i = foldGCM i (+) 0

-- * Ports 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A port is just an address
data Port a = Port Int

-- | A parameter port is a port with a default value.
-- TODO: Document.
data Param a = Param a Int

-- | A goal.
-- TODO: Document.
data Goal a = Goal Int

-- | Can I get a port ID
-- TODO: Document.
class IsPort p where
    portID :: p a -> Int

instance IsPort Port where
    portID (Port id) = id

instance IsPort Param where
    portID (Param _ id) = id

instance IsPort Goal where
    portID (Goal id) = id

