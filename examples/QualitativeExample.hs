module QualitativeExample where

import GCM
import CP
import Compile(runCompare)

type Sign = Int

plus :: (Num a) => a
plus = 1

minus :: (Num a) => a
minus = -1

zero :: (Num a) => a
zero = 0

ambig :: (Num a) => a
ambig = 1000

isAmbig :: CPExp Sign -> CPExp Bool
isAmbig x = (x .> 1) .|| (x .< -1)

times :: Port Sign -> CPExp Sign -> CPExp Sign -> CP ()
times p x y = do 
  vp <- value p
  assert $ vp === (x * y)

add :: Port Sign -> CPExp Sign -> CPExp Sign -> CP ()
add p x y = do
  vp <- value p
  assert $ (x === y) ==> (vp === x)
  assert $ ((x /== y) .&& (x /== 0) .&& (y /== 0)) ==> (vp === ambig)
  assert $ ((x === 0) .|| (y === 0)) ==> (vp === (x + y))

node :: [Port Sign] -> GCM (Port Sign)
node [] = createPort
node xs = constructSum xs

edge :: Sign -> GCM (Port Sign, Port Sign)
edge s = do
  i  <- createPort
  o  <- createPort
  vi <- value i
  component $ times o vi (lit s)
  return (i, o)

constructSum :: [Port Sign] -> GCM (Port Sign)
constructSum [] = do
  p <- createPort
  set p 0
  return p
constructSum (x:xs) = do
  hereResult <- createPort
  restResult <- constructSum xs
  vx <- value x
  vr <- value restResult
  component $ add hereResult vx vr
  return hereResult

linkNodeEdge :: Port Sign
             -> Sign
             -> GCM (Port Sign)
linkNodeEdge n e = do
  (i, o) <- edge e
  link n i
  return o

tinyExample :: GCM ()
tinyExample = do
  a <- node []
  set a plus

  b <- node []
  set b minus

  ao <- linkNodeEdge a minus
  bo <- linkNodeEdge b plus

  c <- node [ao, bo]

  output a "a"
  output b "b"
  output c "c"

main :: IO ()
main = runCompare tinyExample
