-- | Some intended structure to work towards.
--
-- @undefined@s need to be implemented. Some will require us to poke more deeply
-- into the constraint programming side.

module Main where

import Control.Monad
import CP
import GCM
import Port

type M3s = Float

data Action a = Action a
type Param = ParameterPort

prg :: GCM (Port M3s, Param M3s)
prg = do
  pump <- createPort
  cap  <- createParam (5 :: Int)

  component $ do
    x <- value pump
    y <- value cap
    assert $ x `inRange` (0, y)

  return (pump, cap)

icop :: GCM (Action Int)
icop = do
  a <- createAction
  action $ 
    act (*2) a       -- (a -> b) -> CPExpr a -> CPExpr b

  {- or this way:
     action $ 
       cap <- targetOf a
       def <- default cap
       act $ def * 2
  -}

  return a

-- Work to be done:
pump         = undefined
createParam  = undefined
createAction = undefined
action       = undefined
act          = undefined

-- Tidy example.
example :: GCM ()
example = void $ do
  (pmp, cap) <- pump
  a <- icop
  b <- icop
  a `actsUpon` cap
  b `actsUpon` cap
  a `mutex` b

-- ``This is also a difficult one.''
--
--   Maximilian Algehed, Chalmers Uni.
actsUpon :: Action a -> Param a -> GCM ()
actsUpon = undefined

-- See if the action was taken. Powerful stuff.
--
-- (Doing the reflection; this is the hard one)
taken :: Action a -> GCM (Port Int)
taken = undefined

-- Ensure that actions are mutually exclusive.
mutex :: Action a -> Action b -> GCM ()
mutex a1 a2 = do
  p1 <- taken a1
  p2 <- taken a2
  component $ do
    v1 <- value p1
    v2 <- value p2
    assert $ nt ((v1 .> 0) .&& (v2 .> 0))

a .> b = b .< a


main :: IO ()
main = putStrLn "hello"
