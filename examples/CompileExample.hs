{-# LANGUAGE MonadComprehensions #-}
module CompileExample where

import Control.Monad

import Compile0
import Compile1
import GL

import Interfaces.MZPrinter(layout)

compileString ex = do
  putStr $ compileGCM ex
  putStrLn ""

compileMZ ex = do
  let model = compileGCM' ex
  putStrLn "Model: ---------"
  forM_ model print
  putStrLn "Pretty: --------"
  putStrLn (layout model)

example :: GCM ()
example = do
  a <- createPort

  component $ do
    valA <- value a
    assert (valA === lit (1 :: Int))

  output a "a"
