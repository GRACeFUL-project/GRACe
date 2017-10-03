{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor          #-}

module GCM where

import Control.Monad.Writer hiding (Sum)
import Control.Monad.State.Lazy
import Data.Char
import Program
import System.Process

import CP
-- TODO: also re-export CP (or parts of CP) to simplify for users

-- * The GCM monad
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | GRACeFUL Concept Map commands.
data GCMCommand a where
  Output         ::             Port a     -> String -> GCMCommand ()
  CreateVariable :: CPType a => Proxy a    -> GCMCommand (Variable a)
  CreatePort     :: CPType a => Proxy a    -> GCMCommand (Port a)
  -- | Unsure that hardcoding this to Int is a good idea
  CreateGoal     ::             GCMCommand (Goal Int)
  CreateParam    :: CPType a => Proxy a    -> a -> GCMCommand (Param a)
  CreateAction   :: CPType a => Param a    -> GCMCommand (Action a)
  EmbedAction    ::             ActM a     -> GCMCommand ()
  Component      ::             CP a       -> GCMCommand ()
  {-CreateArray1D :: CPType a => Proxy a    -> Int -> GCMCommand (Port (Array1D a))-}
  {-CreateArray2D :: CPType a => Proxy a    -> (Int, Int) -> GCMCommand (Port (Array2D a))-}

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
output :: Port a -> String -> GCM ()
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

-- | Instantiate a variable in the `GCM` monad.
createVariable :: CPType a => GCM (Variable a)
createVariable = Instr (CreateVariable Proxy)

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
act f act@(Action i (Param a j)) = Instr (Act (f (ValueOf $ Var i) (lit a)) act)

-- | Embed a constraint programming component in a `GCM`.
component :: CP a -> GCM ()
component = Instr . Component

-- | A different name for component, as seen in Deliverable 4.3 code examples.
constrain :: CP a -> GCM ()
constrain = component

-- * Derived GCM operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | See if the action was taken.
taken :: CPType a => Action a -> GCM (Port Int)
taken (Action i _) = return (Port $ Var i)

-- | @'link' p1 p2@ creates a connection from port @p1@ to port @p2@.
link :: (CPType a, IsVar p1, IsVar p2) => p1 a -> p2 a -> GCM ()
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

-- | @'linkBy' f p1 p2@ creates a connection from port @p1@ to port @p2@
--          via relation @f@.
linkBy :: (CPType a, CPType b, IsVar pa, IsVar pb, IsVar pi, IsVar po)
    => GCM (pi a, po b)
    -> pa a
    -> pb b
    -> GCM ()
linkBy f a b = do
  (i, o) <- f
  component $ do
    va <- value a
    vb <- value b
    vi <- value i
    vo <- value o
    assert $ va === vi
    assert $ vb === vo

-- | @'set' p v@ sets the value of port @p@ to @v@.
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

-- | @'fun' f@ creates a GCM component for function @f@.
fun :: (CPType a, CPType b) => (CPExp a -> CPExp b) -> GCM (Port a, Port b)
fun f = do
  a <- createPort
  b <- createPort
  component $ do
    va <- value a
    vb <- value b
    assert  $ f va === vb
  return (a, b)

-- * Ports
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A port is just a variable
newtype Port a = Port (Variable a) deriving (Show, Functor)

-- | A parameter port is a port with a default value.
data Param a = Param a (Port a)

-- | A goal contains a variable we'd like to i.e. maximize or minimize.
newtype Goal a = Goal (Variable a)

-- | Instances of IsVar are linkable.
class IsVar v where
  value  :: Monad m => v a -> m (CPExp a)

instance IsVar Variable where
  value = return . ValueOf

instance IsVar Port where
  value (Port v) = value v

instance IsVar Param where
  value (Param _ v) = value v

instance IsVar Goal where
  value (Goal v) = value v
