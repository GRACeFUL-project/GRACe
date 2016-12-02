{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-} 

module GCM 
  ( GCM
  , output, link, createPort, createGoal
  , component , fun, set
  , foldGCM, sumGCM
  , compileGCM, runGCM
  ) where

import Control.Monad.Writer
import Control.Monad.State.Lazy
import CP
import Port
import Program
import System.Process

-- | GRACeFUL Concept Map commands.
data GCMCommand a where
    Output          :: (CPType a) => Port a    -> String -> GCMCommand ()
    CreatePort      :: (CPType a) => Proxy a   -> GCMCommand (Port a)
    CreateGoal      ::               GCMCommand (Goal Int) -- Unsure that hardcoding this to Int is a good idea
    CreateParameter :: (CPType a) => Proxy a   -> a -> GCMCommand (ParameterPort a)
    Component       ::               CP ()     -> GCMCommand ()

-- | A GRACeFUL Concept Map.
type GCM a = Program GCMCommand a

-- * Base operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output :: (CPType a, Show a) => Port a -> String -> GCM ()
output p = Instr . Output p

createPort :: (CPType a) => GCM (Port a)
createPort = Instr (CreatePort Proxy)

createGoal :: GCM (Goal Int)
createGoal = Instr CreateGoal

createParameter :: (CPType a) => a -> GCM (ParameterPort a)
createParameter = Instr . CreateParameter Proxy

component :: CP () -> GCM ()
component = Instr . Component

-- * Derived operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @'link' p1 p2@ creates a connection from port @p1@ to port @p2@.
link :: (CPType a) => Port a -> Port a -> GCM ()
link p1 p2 = component $ do
  v1 <- value p1
  v2 <- value p2
  assert $ v1 === v2

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

foldGCM :: (CPType a, CPType b) 
        => Int 
        -> (CPExp b 
        -> CPExp a -> CPExp b) 
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

sumGCM :: (CPType a, Num a) => Int -> GCM ([Port a], Port a)
sumGCM i = foldGCM i (+) 0

-- Compilation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data CompilationState = CompilationState 
  { outputs      :: [String]
  , expressions  :: [String]
  , declarations :: [String]
  , goals        :: [Int]
  , nextVarId    :: Int
  }

type IntermMonad = State CompilationState

-- Translation
translateGCMCommand :: GCMCommand a -> IntermMonad a
translateGCMCommand = \case
  Output p s -> do
    let i = portID p
    modify $ \st -> st { outputs = outputs st ++ [s ++ " = \\(v" ++ show i ++ ")\\n"]}
  CreatePort proxy -> do
    vid <- gets nextVarId
    let dec = typeDec proxy "var" ++ ": v" ++ show vid ++ ";"
    modify $ \st -> st { nextVarId = vid + 1
                       , declarations = dec : declarations st
                       }
    return $ Port vid
  CreateGoal -> do
    vid <- gets nextVarId 
    let goal = "var int: v" ++ show vid ++ ";"
    modify $ \st -> st { nextVarId = vid + 1
                       , goals = vid : goals st
                       , declarations = goal : declarations st
                       }
    return (Goal vid)
  CreateParameter proxy def -> do
    vid <- gets nextVarId
        -- the value
    let dec = typeDec proxy "var" ++ ": v" ++ show vid ++ ";"
        -- has been acted upon
        dec2 = "var bool: a" ++ show vid ++ ";"
        -- default value
        exp = "constraint ((not a" ++ show vid ++ ") ==> (v" ++ show vid ++ " == " ++ show def ++ "));"
    modify $ \st -> st { nextVarId    = vid + 1
                       , expressions  = exp : expressions st
                       , declarations = dec : dec2 : declarations st
                       }
    return $ ParameterPort def vid
  Component cp -> do
    let (_, exprs) = runWriter $ interpret translateCPCommands cp
    modify $ \st -> st {expressions = expressions st ++ exprs}

-- Final compilation (this function is _very_ ugly!)
compileGCM :: GCM a -> String
compileGCM gcm = stateToString $ flip execState (CompilationState [] [] [] [] 0) $ interpret translateGCMCommand gcm
  where
    makeGoals [] = "\nsolve satisfy;"
    makeGoals gls = "\nsolve maximize ("++ foldl (\s gid -> s++"+v"++ show gid) "0" gls ++ ");"
    stateToString (CompilationState outs exprs declrs goals _) =
      unlines [unlines declrs, unlines exprs] ++ makeGoals goals ++ "\noutput [\""++ concat outs ++"\"];"

-- Crude run function
runGCM :: GCM a -> IO ()
runGCM gcm = do
  writeFile "model.mzn" (compileGCM gcm)
  callCommand "mzn-gecode model.mzn"
  callCommand "rm model.mzn"
