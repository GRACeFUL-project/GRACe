{-# LANGUAGE RecordWildCards #-}

-- | Some intended structure to work towards.
module Main where

import Control.Monad
import GL

-- | A pump has connections for input, output and exports a capacity.
data Pump = Pump
  { pin, pout :: Port  Float
  , cap       :: Param Float
  }

-- | Some virtual pump with a capacity.
pump :: GCM Pump
pump = do
  -- Create ports, and a parameter which controls capacity.
  -- This parameter is exported, and accepts actions operating on
  -- the variable somehow.
  pin  <- createPort
  pout <- createPort
  cap  <- createParam (5 :: Float)

  -- Declare some constraints
  component $ do
    x <- value pin
    z <- value pout
    y <- value cap
    assert $ x `inRange` (0, y) .&& x === z

  return Pump {..}

-- | Increases pump capacity by doubling.
increaseCap :: Param Float -> GCM (Action Float)
increaseCap p = do
  a <- createAction p
  action $           -- Is the `action` thing really necessary? (No, but it creates a separation of concerns)
    act (\a defaultValue -> 2*(i2f a)*defaultValue) a

  {- or this way:
     action $ 
       cap <- targetOf a
       def <- default cap
       act $ def * 2
  -}

  return a

-- Tidy example.
example :: GCM ()
example = void $ do
  -- Declare a pump
  Pump pin pout cap <- pump

  -- Declare two capacity increasing actions
  act1 <- increaseCap cap
  act2 <- increaseCap cap

  -- Declare that actions act1 and act2 are mutually exclusive
  act1 `mutex` act2

main :: IO ()
main = putStrLn "hello"
