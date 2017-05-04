module CLDExample where

import Compile0
import GL

q, p, m :: Num a => a
q = 0
p = 1
m = -1

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

cldNode :: GCM (Port Int)
cldNode = do
  p <- createPort
  component $
    assert  $ val p `inRange` (-1, 1)
  return p

example :: GCM ()
example = do
  a <- cldNode 
  b <- cldNode

  linkBy (relation p) a b

  set a p

  output a "a"
  output b "b"
