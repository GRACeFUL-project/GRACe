{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Compile0 where

import Control.Monad.Writer hiding (Sum)
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import System.Process
import qualified Data.Set as S

import Program
import CP
import GCM

runGCM :: GCM a -> IO String
runGCM gcm = do
  writeFile   "model.mzn" (compileGCM gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", "-1"
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ","
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  callCommand "rm model.mzn model.ozn model.fzn"
  return res

-- Compilation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- TODO: Stages, cleanup

data CompilationState = CompilationState
  { outputs      :: [String]
  , expressions  :: [String]
  , declarations :: [String]
  , goals        :: [Int]
  , nextVarId    :: Int
  , unconParams  :: S.Set Int
  }

type IntermMonad = State CompilationState

unsafeHack "False" = "0"
unsafeHack "True"  = "1"
unsafeHack s       = s

-- | Pretty-printer for `CPExp` expressions.
compileCPExp :: CPExp a -> IntermMonad String
compileCPExp = \case
  ValueOf v  -> return $ "v" ++ show v
  Lit l      -> return $ unsafeHack (show l)
  Equal a b  -> comp2paren a " == " b
  LeThan a b -> comp2paren a " < "  b
  LtEq a b   -> comp2paren a " <= " b
  Add a  b   -> comp2paren a " + "  b
  Mul a  b   -> comp2paren a " * "  b
  Sub a  b   -> comp2paren a " - "  b
  Div a  b   -> comp2paren a " / "  b
  Max a  b   -> do
                  cab <- comp2paren a "," b
                  return $ "max" ++ paren cab
  Min a  b   -> do
                  cab <- comp2paren a "," b
                  return $ "min" ++ paren cab
  And a  b   -> comp2paren a " /\\ "b
  Not a      -> do
                  ac <- compileCPExp a
                  return $ paren $ "not " ++ paren ac
  I2F a      -> do
                  ac <- compileCPExp a
                  return $ "int2float" ++ paren ac
  ForAll m   -> do
                  (bexpr, s) <- runWriterT (compileComprehension m)
                  bexprc     <- compileCPExp bexpr
                  return $ foldr (\outer inner -> "forall" ++ outer ++ "\n" ++ paren inner) bexprc s
  Sum m      -> do
                  (expr, s) <- runWriterT (compileComprehension m)
                  exprc      <- compileCPExp expr
                  return $ foldr (\outer inner -> "sum" ++ outer ++ "\n" ++ paren inner) exprc s
  MaxA m     -> do
                  (expr, s) <- runWriterT (compileComprehension m)
                  exprc      <- compileCPExp expr
                  return $ foldr (\outer inner -> "max" ++ outer ++ "\n" ++ paren inner) exprc s
  MinA m     -> do
                  (expr, s) <- runWriterT (compileComprehension m)
                  exprc      <- compileCPExp expr
                  return $ foldr (\outer inner -> "min" ++ outer ++ "\n" ++ paren inner) exprc s
  {-IdxA1D arr idx -> do-}
    {-idxc <- compileCPExp idx-}
    {-avar <- compileCPExp arr-}
    {-return $ avar ++ "[" ++ idxc ++ "]"-}
  {-IdxA2D arr idx -> do-}
    {-idxcf <- compileCPExp (fst idx)-}
    {-idxcs <- compileCPExp (snd idx)-}
    {-avar  <- compileCPExp arr-}
    {-return $ avar ++ "[" ++ idxcf ++ "," ++ idxcs ++ "]"-}

compileComprehension :: ComprehensionMonad (CPExp a) -> WriterT [String] IntermMonad (CPExp a)
compileComprehension = interpret

instance Interprets (WriterT [String] IntermMonad) ComprehensionCommand where
  interp = translateComprehensionCommand

translateComprehensionCommand :: ComprehensionCommand a -> WriterT [String] IntermMonad a
translateComprehensionCommand (Range (low, high)) = do
  nvar <- lift $ gets nextVarId
  lift $ modify $ \st -> st {nextVarId = nvar + 1}
  lows  <- lift $ compileCPExp low
  highs <- lift $ compileCPExp high
  tell ["(" ++ "v" ++ show nvar ++ " in " ++ lows ++ ".." ++ highs ++ ")"]
  return $ ValueOf (Var nvar)

comp2paren :: (CPType a, CPType b) => CPExp a -> String -> CPExp b -> IntermMonad String
comp2paren a op b = do
  ac <- compileCPExp a
  bc <- compileCPExp b
  return $ paren ac ++ op ++ paren bc

-- | Prenethesise a string
paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | Compiles `CPCommands` to a sequence of `String`s.
translateCPCommands :: CPCommands a -> IntermMonad a
translateCPCommands (Assert bexp) = do
  bexprc <- compileCPExp bexp
  modify $ \st -> st {expressions = expressions st ++ ["constraint " ++ paren bexprc ++ ";"]}
translateCPCommands (CreateLVar proxy) = do
  vid <- gets nextVarId
  let dec = typeDec proxy "var" ++ ": v" ++ show vid ++ ";"
  modify $ \st -> st { nextVarId = vid + 1
                     , declarations = dec : declarations st
                     }
  return $ Var vid

instance Interprets IntermMonad CPCommands where
  interp = translateCPCommands

-- | Compiles `ActCommand`s to a sequence of `String`s.
translateActionCommands :: ActCommand a -> IntermMonad a
translateActionCommands (Act expr (Action i (Param _ (Port j)))) = do
  exprc <- compileCPExp expr
  modify $ \st -> st {expressions = expressions st ++ ["constraint (a"++ show j++" -> (v" ++ show j ++ " == " ++ paren exprc ++ "));"]}

instance Interprets IntermMonad ActCommand where
  interp = translateActionCommands

-- Translation of GCM commands
translateGCMCommand :: GCMCommand a -> IntermMonad a
translateGCMCommand = \case
  Output (Port v) s -> do
    let i = varID v
    modify $ \st -> st { outputs = outputs st ++ ["\\\"" ++ s ++ "\\\"" ++ " : \\(v" ++ show i ++ ")"]}
  CreatePort proxy -> do
    vid <- gets nextVarId
    let dec = typeDec proxy "var" ++ ": v" ++ show vid ++ ";"
    modify $ \st -> st { nextVarId = vid + 1
                       , declarations = dec : declarations st
                       }
    return $ Port (Var vid)
  CreateGoal -> do
    vid <- gets nextVarId
    let goal = "var int: v" ++ show vid ++ ";"
    modify $ \st -> st { nextVarId = vid + 1
                       , goals = vid : goals st
                       , declarations = goal : declarations st
                       }
    return (Goal $ Var vid)
  CreateParam proxy def -> do
    vid <- gets nextVarId
        -- the value
    let dec = typeDec proxy "var" ++ ": v" ++ show vid ++ ";"
        -- has been acted upon
        dec2 = "var bool: a" ++ show vid ++ ";"
        -- default value
        exp = "constraint ((not a" ++ show vid ++ ") -> (v" ++ show vid ++ " == " ++ show def ++ "));"
    modify $ \st -> st { nextVarId    = vid + 1
                       , expressions  = exp : expressions st
                       , declarations = dec : dec2 : declarations st
                       , unconParams  = S.insert vid (unconParams st)
                       }
    return $ Param def (Port $ Var vid)
  CreateAction p@(Param a (Port j)) -> do
    vid <- gets nextVarId
    let dec = "var int: v" ++ show vid ++ ";"
        exp = "constraint (v" ++ show vid ++ ">= 0);"
        exp2 = "constraint ((v" ++ show vid ++ "> 0) -> a" ++ show j ++ ");"
    modify $ \st -> st { nextVarId    = vid + 1
                       , expressions  = exp : exp2 : expressions st
                       , declarations = dec : declarations st
                       , unconParams  = S.delete (varID j) (unconParams st)
                       }
    return $ Action vid p
  Component cp     -> void $ interpret cp
  EmbedAction actm -> void $ interpret actm
  {-CreateArray1D proxy len -> do-}
    {-vid <- gets nextVarId-}
    {-let dec = (typeDec proxy $ "array[0.." ++ show (len - 1) ++"] of var") ++ ": v" ++ show vid ++ ";"-}
    {-modify $ \st -> st { nextVarId = vid + 1-}
                       {-, declarations = dec : declarations st-}
                       {-}-}
    {-return $ Port vid-}
  {-CreateArray2D proxy (i, j) -> do-}
    {-vid <- gets nextVarId-}
    {-let dec = (typeDec proxy $ "array[0.." ++ show (i - 1) ++ ",0.." ++ show (j - 1) ++ "] of var") ++ ": v" ++ show vid ++ ";"-}
    {-modify $ \st -> st { nextVarId = vid + 1-}
                       {-, declarations = dec : declarations st-}
                       {-}-}
    {-return $ Port vid-}

instance Interprets IntermMonad GCMCommand where
  interp = translateGCMCommand

-- | Compiles a `GCM a` to a MiniZinc program
compileGCM :: GCM a -> String
compileGCM gcm = stateToString $ flip execState (CompilationState [] [] [] [] 0 S.empty) $ interpret gcm
  where
    makeGoals [] = "\nsolve satisfy;"
    makeGoals gls = "\nsolve maximize ("++ foldl (\s gid -> s++"+v"++ show gid) "0" gls ++ ");"
    stateToString (CompilationState outs exprs declrs goals _ uncon) =
      unlines [unlines declrs, unlines exprs]
      ++ (makeUncon (S.toList uncon))
      ++ makeGoals goals
      ++ "\noutput [\"{"
      ++(intercalate ",\\n" outs)
      ++ "}\"];"
    makeUncon [] = ""
    makeUncon (i:uncons) = "constraint (not a"++show i++");\n" ++ (makeUncon uncons)
