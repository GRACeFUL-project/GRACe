{-# LANGUAGE MonadComprehensions #-}
module ForAllExample where

import Compile0
import GCM
import CP

-- TODO: note that arrays are currently not supported

main = do
  putStr $ compileGCM example
  putStrLn ""
  runGCM example

-- | A rather meaningless example...
example :: GCM ()
example = do
  v <- createVariable :: GCM (Variable Int)
  a <- createArray 5  :: GCM (Variable (Array1D Int))

  component $ do
    v <- value v
    a <- value a
    assert $ v `inRange` (7, 8)
    assert $ forAll [(a .!! i) `inRange` (0, v) | i <- 0 ... 4]
    assert $ 211 .> summ [i + j | i <- 0 ... 4, j <- 1 ... v]

  output v "v"
  output a "a"
