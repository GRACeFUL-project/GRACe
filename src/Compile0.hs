{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE GADTs                #-}
module Compile0 where

import Control.Monad.Writer
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import System.Process
import qualified Data.Set as S

import Program
import GL

runGCM :: GCM a -> IO ()
runGCM gcm = do
  writeFile   "model.mzn" (compileGCM gcm)
  callCommand "mzn2fzn model.mzn"
  callCommand "fzn-gecode -p 4 -n 10 model.fzn | solns2out --soln-sep \"\" --search-complete-msg \"\" model.ozn"
  callCommand "rm model.mzn model.ozn model.fzn"

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

type VarName = Int
type CPIntermMonad = State VarName

-- | Pretty-printer for `CPExp` expressions.
compileCPExp :: CPType a => CPExp a -> CPIntermMonad String
compileCPExp = \case
    ValueOf p  -> return $ "v" ++ show (portID p)
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
                    (bexpr, s) <- runWriterT (compileForAll m)
                    bexprc     <- compileCPExp bexpr
                    return $ foldr (\outer inner -> outer ++ "\n" ++ paren inner) bexprc s

compileForAll :: ForAllMonad (CPExp Bool) -> WriterT [String] CPIntermMonad (CPExp Bool)
compileForAll = interpret translateForAllCommand 

translateForAllCommand :: ForAllCommand a -> WriterT [String] CPIntermMonad a
translateForAllCommand (Range (low, high)) =
  do
    nvar <- lift get
    lift $ put (nvar+1)
    lows  <- lift $ compileCPExp low
    highs <- lift $ compileCPExp high
    tell ["forall(" ++ "v" ++ show nvar ++ " in " ++ lows ++ ".." ++ highs ++ ")"]
    return $ ValueOf (Port nvar)

comp2paren :: (CPType a, CPType b) => CPExp a -> String -> CPExp b -> CPIntermMonad String
comp2paren a op b = do
  ac <- compileCPExp a
  bc <- compileCPExp b
  return $ paren ac ++ op ++ paren bc

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | Compiles `CPCommands` to a sequence of `String`s.
translateCPCommands :: CPCommands a -> WriterT [String] CPIntermMonad a
translateCPCommands (Assert bexp) = do
  bexprc <- lift $ compileCPExp bexp
  tell ["constraint " ++ paren bexprc ++ ";"]

translateActionCommands :: ActCommand a -> WriterT [String] CPIntermMonad a
translateActionCommands (Act expr (Action i (Param _ j))) = do
  exprc <- lift $ compileCPExp expr
  tell ["constraint (a"++ show j++" -> (v" ++ show j ++ " == " ++ paren exprc ++ "));"]

-- Translation
translateGCMCommand :: GCMCommand a -> IntermMonad a
translateGCMCommand = \case
  Output p s -> do
    let i = portID p
    modify $ \st -> st { outputs = outputs st ++ ["\\\"" ++ s ++ "\\\"" ++ " : \\(v" ++ show i ++ ")"]}
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
    return $ Param def vid
  CreateAction p@(Param a j) -> do
    vid <- gets nextVarId
    let dec = "var int: v" ++ show vid ++ ";"
        exp = "constraint (v" ++ show vid ++ ">= 0);"
        exp2 = "constraint ((v" ++ show vid ++ "> 0) -> a" ++ show j ++ ");"
    modify $ \st -> st { nextVarId    = vid + 1
                       , expressions  = exp : exp2 : expressions st
                       , declarations = dec : declarations st
                       , unconParams  = S.delete j (unconParams st)
                       }
    return $ Action vid p
  Component cp -> do
    vid <- fmap nextVarId get 
    let ((_, exprs), nvid) = flip runState vid $ runWriterT $ interpret translateCPCommands cp
    modify $ \st -> st {expressions = expressions st ++ exprs, nextVarId = nvid}
  EmbedAction actm -> do
    vid <- fmap nextVarId get 
    let ((_, exprs), nvid) = flip runState vid $ runWriterT $ interpret translateActionCommands actm
    modify $ \st -> st {expressions = expressions st ++ exprs, nextVarId = nvid}

-- Final compilation (this function is _very_ ugly!)
compileGCM :: GCM a -> String
compileGCM gcm = stateToString $ flip execState (CompilationState [] [] [] [] 0 S.empty) $ interpret translateGCMCommand gcm
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
