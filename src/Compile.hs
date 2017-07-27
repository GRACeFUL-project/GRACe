{-# LANGUAGE MonadComprehensions #-}

module Compile where
import Control.Monad

import Compile0
import Compile1
import GCM
import CP

import Interfaces.MZPrinter(layout)

-- Print minizinc code produced by old compiler.
compileString :: GCM a -> IO ()
compileString ex = do
  putStrLn "Old compiler: --------"
  putStr $ compileGCM ex
  putStrLn ""

-- Print minizinc model produced by haskelzinc compiler.
compileMZ :: GCM a -> IO ()
compileMZ ex = do
  let modl = compileGCM' ex
  putStrLn ""
  putStrLn "Haskelzinc Model: --------"
  forM_ modl print
  putStrLn ""
  putStrLn "Haskelzinc Pretty: --------"
  putStrLn (layout modl)

compileCompare :: GCM a -> IO ()
compileCompare ex = do
  compileString ex
  compileMZ ex

-- Run a GRACe program, setting the first argument to True for haskelzinc
-- compiler, False for old compiler.
run :: Bool -> GCM a -> IO String
run True p  = runGCM' p
run False p = runGCM p

runCompare :: GCM a -> IO ()
runCompare ex = do
  putStrLn ""
  putStrLn "Results via old compiler:"
  putStr =<< runGCM ex
  putStrLn "Results via haskelzinc:"
  putStr =<< runGCM' ex

example :: GCM ()
example = do
  a <- createPort

  component $ do
    valA <- value a
    assert (valA === lit (1 :: Int))

  output a "a"

test :: IO ()
test = do
  compileCompare example
  runCompare example
  putStr =<< run True example
  putStr =<< run False example
