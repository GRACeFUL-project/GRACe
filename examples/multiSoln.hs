{-#LANGUAGE ScopedTypeVariables #-}
import Library
import CLDlib
import Compile0
import Compile1
import Compile
import qualified Interfaces.MZPrinter as HZPrinter
import System.Process
import Control.Monad.State.Lazy

runWithHZ :: Bool -> GCM a -> IO String
runWithHZ showAll gcm = do
  writeFile   "model.mzn" (HZPrinter.layoutModel $ compileGCM' gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", k
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ","
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  callCommand "rm model.mzn model.ozn model.fzn"
  return res
  where k = case showAll of
          True -> param
          False -> "-1"

runComp :: Bool -> GCM a -> IO String
runComp showAll gcm = do
  writeFile   "model.mzn" (compileGCM gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", k
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ""
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  callCommand "rm model.mzn model.ozn model.fzn"
  return res
  where k = case showAll of
          True -> param
          False -> "-1"

-- | Run using both compilers to compare results.
runCompare' :: Bool -> GCM a -> IO ()
runCompare' showAll ex = do
  putStrLn ""
  putStrLn "Results via old compiler:"
  putStr =<< runComp showAll ex
  putStrLn "Results via haskelzinc:"
  putStr =<< runWithHZ showAll ex

simple :: GCM ()
simple = do
  (p1 :: Port Sign) <- createPort
  (p2 :: Port Sign) <- createPort
  (s :: Port Int) <- createPort
  g <- createIntGoal
  link s g
  component $ do
    v1 <- value p1
    v2 <- value p2
    vs <- value s
    assert $ (v1 === 0 .&& v2 /== 0) ==> (vs === 5)
    assert $ (v2 === 0 .&& v1 /== 0) ==> (vs === 5)
    assert $ (v1 /== 0 .&& v2 /== 0) ==> (vs === 0)
    assert $ (v1 === 0 .&& v2 === 0) ==> (vs === 10)
    assert $ v1 === 2 .|| v2 === 2
  output p1 "p1"
  output p2 "p2"
  output s  "s"

param :: String
param = "20"

main :: IO ()
main = do
  runCompare' True simple
  --compileString simple
  --compileMZ simple
