module QualitativeExample where

import GCM
import CP
import Compile0

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

-- TODO: migrate to new internal structure (GCP + CP)
times :: Port Sign -> CPExp Sign -> CPExp Sign -> CP ()
times p x y = assert $ val p === (x * y)

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
  i <- createPort
  o <- createPort
  component $ times o (val i) (lit s)
  return (i, o)

constructSum :: [Port Sign] -> GCM (Port Sign)
constructSum [] = do
  p <- createPort
  set p 0
  return p
constructSum (x:xs) = do
  hereResult <- createPort
  restResult <- constructSum xs
  component $ add hereResult (val x) (val restResult)
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
