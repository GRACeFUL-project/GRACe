{-# LANGUAGE MonadComprehensions #-}
module ForAllExample where

import Compile0
import GL

main = do
  putStr $ compileGCM example
  putStrLn ""
  runGCM example

example :: GCM ()
example = do
  v <- createVariable :: GCM (Variable Int)

  component $ do
    v <- value v
    assert $ v `inRange` (7, 100)
    assert $ forAll [i .< v | i <- 10 ... v]

  output v "v"
