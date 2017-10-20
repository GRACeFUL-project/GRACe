{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes #-}
module Compile1 where

import Control.Monad.Writer hiding (Sum)
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import System.Process
import qualified Data.Set as S

import Program
import CP
import GCM

import qualified Interfaces.MZAST as HZast
import qualified Interfaces.MZASTBase as HZ
import qualified Interfaces.MZBuiltIns as HZBuiltIns
import qualified Interfaces.MZPrinter as HZPrinter

-- Compilation via haskelzinc
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- TODO: Rebase, along with Compile0 and Compile, remove redundancies.

debug = True

runGCM' :: GCM a -> IO String
runGCM' gcm = do
  writeFile   "model.mzn" (HZPrinter.layoutModel $ compileGCM' gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", "-1"
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ","
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  unless debug $ callCommand "rm model.mzn model.ozn model.fzn"
  return res

data CompilationState = CompilationState
  { model             :: HZ.MZModel
  , goals             :: [Int]
  , outputs           :: [(String, String)]
  , nextVarId         :: Int
  }

type IntermMonad = State CompilationState

compileCPExp' :: CPExp a -> IntermMonad HZ.Expr
compileCPExp' = \case
    ValueOf v  -> return $ HZ.Var (varName $ varID v)
    Lit l      -> return $ hzConst l
    Equal a b  -> applyOp2 a b (HZBuiltIns.=.=)
    LeThan a b -> applyOp2 a b (HZBuiltIns.<.)
    LtEq a b   -> applyOp2 a b (HZBuiltIns.<=.)
    Add a  b   -> applyOp2 a b (HZBuiltIns.+.)
    Mul a  b   -> applyOp2 a b (HZBuiltIns.*.)
    Sub a  b   -> applyOp2 a b (HZBuiltIns.-.)
    Div a  b   -> applyOp2 a b (HZBuiltIns./.)
    Max a  b   -> applyOp2 a b (listOpToOp2 HZBuiltIns.mz_max)
    Min a  b   -> applyOp2 a b (listOpToOp2 HZBuiltIns.mz_min)
    And a  b   -> applyOp2 a b (HZBuiltIns./\.)
    Not a      -> applyOp1 a HZBuiltIns.not_
    I2F a      -> applyOp1 a (listOpToOp1 HZBuiltIns.mz_int2float)
    ForAll m   -> genCall m "forall"
    Sum m      -> genCall m "sum"
    MaxA m     -> genCall m "max"
    MinA m     -> genCall m "min"

genCall :: ComprehensionMonad (CPExp a) -> String -> IntermMonad (HZ.Expr)
genCall m call = do
      (bexpr, s) <- runWriterT (compileComprehension m)
      bexprc <- compileCPExp' bexpr
      return $ HZast.forall s call bexprc

listOpToOp1 :: ([HZ.Expr] -> HZ.Expr) -> (HZ.Expr -> HZ.Expr)
listOpToOp1 f = f . return

listOpToOp2 :: ([HZ.Expr] -> HZ.Expr) -> (HZ.Expr -> HZ.Expr -> HZ.Expr)
listOpToOp2 f = curry $ f . tupleToList2
  where tupleToList2 (x,y) = [x,y]

applyOp1 :: (CPType a) => CPExp a -> (HZ.Expr -> HZ.Expr) -> IntermMonad HZ.Expr
applyOp1 a op = op <$> compileCPExp' a

applyOp2 :: (CPType a, CPType b) => CPExp a -> CPExp b -> (HZ.Expr -> HZ.Expr -> HZ.Expr) -> IntermMonad HZ.Expr
applyOp2 a b op = op <$> compileCPExp' a <*> compileCPExp' b

compileComprehension :: ComprehensionMonad (CPExp a) -> WriterT [HZ.CompTail] IntermMonad (CPExp a)
compileComprehension = interpret

instance Interprets (WriterT [HZ.CompTail] IntermMonad) ComprehensionCommand where
  interp = translateComprehensionCommand'

translateComprehensionCommand' :: ComprehensionCommand a -> WriterT [HZ.CompTail] IntermMonad a
translateComprehensionCommand' (Range (low, high)) = do
  nvar <- lift getAndIncrVar
  lows <- lift $ compileCPExp' low
  highs <- lift $ compileCPExp' high
  let compTail = [varNameStr nvar] HZast.@@ lows HZBuiltIns.... highs
  tell [compTail]
  return (ValueOf (Var nvar))

translateCPCommands' :: CPCommands a -> IntermMonad a
translateCPCommands' (Assert bexp) = do
  bexprc <- compileCPExp' bexp
  let line = constr bexprc
  modify $ \st -> st {model = model st ++ [line]}
translateCPCommands' (CreateLVar proxy) = do
  vid <- getAndIncrVar
  let varType = hzType proxy
  let line = declVar varType (varNameStr vid)
  modify $ \st -> st { model = model st ++ [line]
                     }
  return (Var vid)

instance Interprets IntermMonad CPCommands where
  interp = translateCPCommands'

translateActionCommands' :: ActCommand a -> IntermMonad a
translateActionCommands' (Act expr (Action _ (Param _ (Port j)))) = do
  exprc <- compileCPExp' expr
  let line = constr (HZ.Var (actName $ varID j) HZBuiltIns.->. (HZ.Var (varName $ varID j) HZBuiltIns.=.= exprc))
  modify $ \st -> st { model = model st ++ [line]
                     }

instance Interprets IntermMonad ActCommand where
  interp = translateActionCommands'

getAndIncrVar :: (MonadState CompilationState m) => m Int
getAndIncrVar = do
  x <- gets nextVarId
  modify $ \st -> st { nextVarId = x + 1 }
  return x

translateGCMCommand' :: GCMCommand a -> IntermMonad a
translateGCMCommand' = \case
  Output (Port v) s -> do
    let i = varID v
    modify $ \st -> st { outputs = outputs st ++ [(s, varNameStr i)] }
  CreatePort proxy -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let line = declVar varType (varNameStr vid)
    modify $ \st -> st { model = model st ++ [line]
                       }
    return $ Port (Var vid)
  CreateGoal -> do
    vid <- getAndIncrVar
    let varType = HZ.Float
    let line = declVar varType (varNameStr vid)
    modify $ \st -> st { goals = vid : goals st
                       , model = model st ++ [line]
                       }
    return $ Goal (Var vid)
  CreateParam proxy def -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let lines = [ declVar varType (varNameStr vid)
                , declVar HZ.Bool (actNameStr vid)
                , constr (HZBuiltIns.not_ (HZ.Var (actName vid)) HZBuiltIns.->. (HZ.Var (varName vid) HZBuiltIns.=.= hzConst def))
                ]
    modify $ \st -> st { model = model st ++ lines
                       }
    return $ Param def (Port (Var vid))
  CreateAction p@(Param _ (Port j)) -> do
    vid <- getAndIncrVar
    let varType = HZ.Int
    let lines = [ declVar varType (varNameStr vid)
                , constr (HZ.Var (varName vid) HZBuiltIns.>=. HZ.IConst 0)
                , constr ((HZ.Var (varName vid) HZBuiltIns.>. HZ.IConst 0) HZBuiltIns.->. HZ.Var (actName $ varID j))
                ]
    modify $ \st -> st { model = model st ++ lines
                       }
    return $ Action vid p
  Component cp -> void $ interpret cp
  EmbedAction actm -> void $ interpret actm

instance Interprets IntermMonad GCMCommand where
  interp = translateGCMCommand'

compileGCM' :: GCM a -> HZ.MZModel
compileGCM' gcm = stateToModel $ flip execState (CompilationState [] [] [] 0) $ interpret gcm
  where
    stateToModel cs =
      model cs ++
      [HZast.Output $ HZ.ArrayLit $ makeOutputs $ outputs cs] ++
      [makeGoals $ goals cs]
    makeOutputs os = HZast.string "{" : outputsToExpr os ++ [HZast.string "}"]
    -- the text is quoted with single quotes until \" is added to haskelzinc
    outputToExpr (text, vname) = [HZast.string $ "'" ++ text ++ "' : ", HZBuiltIns.mz_show [HZast.Var (HZ.Simpl vname)]]
    outputsToExpr os = intercalate [HZast.string ",\n"] (map outputToExpr os)
    makeGoals [] = HZ.Solve $ HZ.Satisfy []
    makeGoals gls = HZ.Solve $ HZ.Maximize [] (foldl (HZBuiltIns.+.) (HZast.int 0) (map intToVar gls))

intToVar :: Int -> HZ.Expr
intToVar = HZ.Var . varName

hzZero :: HZ.Type -> HZ.Expr
hzZero HZ.Int = HZ.IConst 0
hzZero HZ.Float = HZ.FConst 0
hzZero HZ.Bool = HZ.BConst False

varName :: Int -> HZ.Ident
varName = HZ.Simpl . varNameStr

varNameStr :: Int -> String
varNameStr i = 'v' : show i

actName :: Int -> HZ.Ident
actName = HZ.Simpl . actNameStr

actNameStr :: Int -> String
actNameStr i = 'a' : show i

constr :: HZ.Expr -> HZ.Item
constr e = HZast.turnToItem $ HZast.constraint e

declVar :: HZ.Type -> String -> HZ.Item
declVar t s = HZast.declare $ HZast.declareOnly $ HZast.variable HZ.Dec t s
