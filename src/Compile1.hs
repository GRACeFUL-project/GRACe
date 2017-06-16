{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
module Compile1 where

import Control.Monad.Writer hiding (Sum)
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import System.Process
import qualified Data.Set as S

import Program
import GL hiding ((...))

import Interfaces.MZAST
import Interfaces.MZASTBase
import Interfaces.MZBuiltIns
import Interfaces.MZPrinter

runGCM' :: GCM a -> IO ()
runGCM' gcm = do
  writeFile   "model.mzn" (layout $ compileGCM' gcm)
  callCommand "mzn2fzn model.mzn"
  callCommand "fzn-gecode -p 4 -n 10 model.fzn | solns2out --soln-sep \"\" --search-complete-msg \"\" model.ozn"
  callCommand "rm model.mzn model.ozn model.fzn"

data CompilationState = CompilationState
  { model             :: MZModel
  , goals             :: [Int]
  , outputs           :: [(String, String)]
  , nextVarId         :: Int
  }

type IntermMonad = State CompilationState

compileCPExp' :: CPExp a -> IntermMonad Expr
compileCPExp' = \case
    ValueOf p  -> return $ Var (varName (portID p))
    Lit l      -> return $ hzConst l
    Equal a b  -> applyOp2 a b (=.=)
    LeThan a b -> applyOp2 a b (<.)
    LtEq a b   -> applyOp2 a b (<=.)
    Add a  b   -> applyOp2 a b (+.)
    Mul a  b   -> applyOp2 a b (*.)
    Sub a  b   -> applyOp2 a b (-.)
    Div a  b   -> applyOp2 a b (/.)
    Max a  b   -> applyOp2 a b (listOpToOp2 mz_max)
    Min a  b   -> applyOp2 a b (listOpToOp2 mz_min)
    And a  b   -> applyOp2 a b (/\.)
    Not a      -> applyOp1 a not_
    I2F a      -> applyOp1 a (listOpToOp1 mz_int2float)
    ForAll m   -> genCall m "forall"
    Sum m      -> genCall m "sum"
    MaxA m     -> genCall m "max"
    MinA m     -> genCall m "min"
    IdxA1D arr idx -> do
      idxc <- compileCPExp' idx
      avar <- compileCPExp' arr
      return $ unsafeArrayAccess avar [idxc]
    IdxA2D arr idx -> do
      idxcf <- compileCPExp' (fst idx)
      idxcs <- compileCPExp' (snd idx)
      avar <- compileCPExp' arr
      return $ unsafeArrayAccess avar [idxcf, idxcs]

-- TODO: figure out a better alternative?
unsafeArrayAccess (Var arr) y = arr !. y
unsafeArrayAccess x _ = error $ "accessing something which is not a var: " ++ show x

genCall m call = do
      (bexpr, s) <- runWriterT (compileComprehension m)
      bexprc <- compileCPExp' bexpr
      return $ forall s call bexprc

listOpToOp1 :: ([Expr] -> Expr) -> (Expr -> Expr)
listOpToOp1 f = f . return

listOpToOp2 :: ([Expr] -> Expr) -> (Expr -> Expr -> Expr)
listOpToOp2 f = curry $ f . tupleToList2
  where tupleToList2 (x,y) = [x,y]

applyOp1 :: (CPType a) => CPExp a -> (Expr -> Expr) -> IntermMonad Expr
applyOp1 a op = op <$> compileCPExp' a

applyOp2 :: (CPType a, CPType b) => CPExp a -> CPExp b -> (Expr -> Expr -> Expr) -> IntermMonad Expr
applyOp2 a b op = op <$> compileCPExp' a <*> compileCPExp' b

compileComprehension :: ComprehensionMonad (CPExp a) -> WriterT [CompTail] IntermMonad (CPExp a)
compileComprehension = interpret

instance Interprets (WriterT [CompTail] IntermMonad) ComprehensionCommand where
  interp = translateComprehensionCommand'

translateComprehensionCommand' :: ComprehensionCommand a -> WriterT [CompTail] IntermMonad a
translateComprehensionCommand' (Range (low, high)) = do
  nvar <- lift getAndIncrVar
  lows <- lift $ compileCPExp' low
  highs <- lift $ compileCPExp' high
  let compTail = [varName nvar] @@ lows ... highs
  tell [compTail]
  return (ValueOf (Port nvar))

translateCPCommands' :: CPCommands a -> IntermMonad a
translateCPCommands' (Assert bexp) = do
  bexprc <- compileCPExp' bexp
  let line = constraint bexprc
  modify $ \st -> st {model = model st ++ [line]}
translateCPCommands' (CreateLVar proxy) = do
  vid <- getAndIncrVar
  let varType = hzType proxy
  let line = declare $ variable Dec varType (varName vid)
  modify $ \st -> st { model = model st ++ [line]
                     }
  return (Port vid)

instance Interprets IntermMonad CPCommands where
  interp = translateCPCommands'

translateActionCommands' :: ActCommand a -> IntermMonad a
translateActionCommands' (Act expr (Action i (Param _ j))) = do
  exprc <- compileCPExp' expr
  let line = constraint (Var (actName j) ->. (Var (varName j) =.= exprc))
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
  GL.Output p s -> do
    let i = portID p
    modify $ \st -> st { outputs = outputs st ++ [(s, varName i)] }
  CreatePort proxy -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let line = Declare $ Declaration (Variable (Dec, varType, varName vid)) [] Nothing
    modify $ \st -> st { model = model st ++ [line]
                       }
    return (Port vid)
  CreateGoal -> do
    vid <- getAndIncrVar
    let varType = Int
    let line = declare $ variable Dec varType (varName vid)
    modify $ \st -> st { goals = vid : goals st
                       , model = model st ++ [line]
                       }
    return (Goal vid)
  CreateParam proxy def -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let lines = [ declare $ variable Dec varType (varName vid)
                , declare $ variable Dec Bool (actName vid)
                , constraint (not_ (Var (actName vid)) ->. (Var (varName vid) =.= hzConst def))
                ]
    modify $ \st -> st { model = model st ++ lines
                       }
    return (Param def vid)
  CreateAction p@(Param a j) -> do
    vid <- getAndIncrVar
    let varType = Int
    let lines = [ declare $ variable Dec varType (varName vid)
                , constraint (Var (varName vid) >=. IConst 0)
                , constraint ((Var (varName vid) >. IConst 0) ->. Var (actName j))
                ]
    modify $ \st -> st { model = model st ++ lines
                       }
    return (Action vid p)
  Component cp -> void $ interpret cp
  EmbedAction actm -> void $ interpret actm
  CreateArray1D proxy len -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let mzZero = hzZero varType
    let mzLen = hzConst (len - 1)
    let line = declare $ variable Dec (Array [CT $ mzZero ... mzLen] Dec varType) (varName vid)
    modify $ \st -> st { model = model st ++ [line]}
    return (Port vid)
  CreateArray2D proxy (i, j) -> do
    vid <- getAndIncrVar
    let varType = hzType proxy
    let mzZero = hzZero varType
    let mzLenI = hzConst (i - 1)
    let mzLenJ = hzConst (j - 1)
    let line = declare $ variable Dec (Array [CT $ mzZero ... mzLenI, CT $ mzZero ... mzLenJ] Dec varType) (varName vid)
    modify $ \st -> st { model = model st ++ [line]}
    return (Port vid)

instance Interprets IntermMonad GCMCommand where
  interp = translateGCMCommand'

compileGCM' :: GCM a -> MZModel
compileGCM' gcm = stateToModel $ flip execState (CompilationState [] [] [] 0) $ interpret gcm
  where
    makeGoals [] = Solve $ Satisfy []
    makeGoals gls = Solve $ Maximize [] (foldl (+.) (int 0) (map intToVar gls))
    -- the text is quoted with single quotes until \" is added to haskelzinc
    outputToExpr (text,varName) = [string $ "'" ++ text ++ "' : ", mz_show [var varName]]
    outputsToExpr outputs = intercalate [string ",\n"] (map outputToExpr outputs)
    makeOutputs outputs = string "{" : outputsToExpr outputs ++ [string "}"]
    stateToModel (CompilationState model goals outputs _) =
      model ++
      [Interfaces.MZAST.Output $ ArrayLit $ makeOutputs outputs] ++
      [makeGoals goals]

intToVar :: Int -> Expr
intToVar i = Var (varName i)

hzZero :: Type -> Expr
hzZero Int = IConst 0
hzZero Float = FConst 0
hzZero Bool = BConst False

varName :: Int -> String
varName i = 'v' : show i

actName :: Int -> String
actName i = 'a' : show i

