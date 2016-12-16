{-# LANGUAGE LambdaCase#-}
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
    writeFile "model.mzn" (compileGCM gcm)
    callCommand "mzn-gecode -p 4 -n 10 model.mzn"
    callCommand "rm model.mzn"

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

-- | Pretty-printer for `CPExp` expressions.
compileCPExp :: CPType a => CPExp a -> String
compileCPExp = \case
    ValueOf p  -> "v" ++ show (portID p)
    Lit l      -> map toLower $ show l
    Equal a b  -> comp2paren a " == " b
    LeThan a b -> comp2paren a " < "  b
    LtEq a b   -> comp2paren a " <= " b
    Add a  b   -> comp2paren a " + "  b
    Mul a  b   -> comp2paren a " * "  b
    Sub a  b   -> comp2paren a " - "  b
    Div a  b   -> comp2paren a " / "  b
    Max a  b   -> "max" ++ paren (comp2paren a "," b)
    Min a  b   -> "min" ++ paren (comp2paren a "," b)
    And a  b   -> comp2paren a " /\\ "b
    Not a      -> paren $ "not " ++ paren (compileCPExp a)
    I2F a      -> "int2float" ++ paren (compileCPExp a)

comp2paren :: (CPType a, CPType b) => CPExp a -> String -> CPExp b -> String
comp2paren a op b = paren (compileCPExp a) ++ op ++ paren (compileCPExp b)

paren :: String -> String
paren s = "(" ++ s ++ ")"

-- | Compiles `CPCommands` to a sequence of `String`s.
translateCPCommands :: CPCommands a -> Writer [String] a
translateCPCommands (Assert bexp) =
    tell ["constraint " ++ paren (compileCPExp bexp) ++ ";"]

translateActionCommands :: ActCommand a -> Writer [String] a
translateActionCommands (Act expr (Action i (Param _ j))) =
    tell ["constraint (a"++ show j++" -> (v" ++ show j ++ " == " ++ paren (compileCPExp expr) ++ "));"]

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
        let (_, exprs) = runWriter $ interpret translateCPCommands cp
        modify $ \st -> st {expressions = expressions st ++ exprs}
    EmbedAction actm -> do
        let (_, exprs) = runWriter $ interpret translateActionCommands actm
        modify $ \st -> st {expressions = expressions st ++ exprs}

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
