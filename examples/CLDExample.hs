module CLDExample where

import Compile0(runGCM)
import GCM     (GCM, component, output, Port, createPort, linkBy, value, set)
import CP      (assert, lit, nt, (===), (==>), inRange)

-- Labels for the relations (arrows) in a Causal Loop Diagram
q, p, m :: Num a => a
q =  0  -- ? = question mark
p =  1  -- + = plus
m = -1  -- - = minus

relation :: Int -> GCM (Port Int, Port Int)
relation s = do
  pi <- createPort
  po <- createPort

  component $ do
    i <- value pi
    o <- value po
    assert $    (i === q) ==>    (o === q)
    assert $ nt (i === q) ==> nt (o === negate (lit s * i))

  return (pi, po)

-- We can make a "+" link an infix operator:
(-+>) :: Port Int -> Port Int -> GCM ()
(-+>) = linkBy (relation p)

cldNode :: GCM (Port Int)
cldNode = do
  p <- createPort
  component $ do
    pv <- value p
    assert  $ pv `inRange` (-1, 1)
  return p


example :: GCM ()
example = do
  a <- cldNode
  b <- cldNode

  a -+> b

  set a p

  output a "a"
  output b "b"

main = runGCM example >>= putStr
-- TODO: Check the result and explain the example
