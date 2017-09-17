module CLD where
import GCM
import CP
import Compile
import Sign

add :: Port Sign -> CPExp Sign -> CPExp Sign -> CP ()
add p x y = do
  vp <- value p
  assert $ (x === y) ==> (vp === x)
  assert $ ((x /== y) .&& (x /== Lit Z) .&& (y /== Lit Z)) ==> (vp === Lit Q)
  assert $ ((x === Lit Z) .|| (y === Lit Z)) ==> (vp === (x + y)) 

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

pArrow :: GCM (Port Sign, Port Sign)
pArrow = do
  p <- createPort
  q <- createPort
  link p q
  return (p, q)

mArrow :: GCM (Port Sign, Port Sign)
mArrow = do
  p <- createPort
  q <- createPort
  component $ do
    pv <- value p
    qv <- value q
    assert $ qv === (-1) * pv
  return (p, q)

(-+>) :: Port Sign -> Port Sign -> GCM ()
p -+> q = do
 (p0, q0) <- pArrow
 link p p0
 link q q0

(-->) :: Port Sign -> Port Sign -> GCM ()
p --> q = do
 (p0, q0) <- mArrow
 link p p0
 link q q0

cldNode :: Maybe Sign -> Int -> GCM ([Port Sign], Port Sign)
cldNode obsSign n = do
  outPort <- createPort
  inPorts <- mapM (\_ -> createPort) [1..n]
  influence <- constructSum inPorts
  component $ do
    i <- value influence
    o <- value outPort
    case obsSign of
      Just s -> do
        assert $ (o === lit s)
        if n > 0
          then (assert $ (i === lit s))
          else return ()
      Nothing -> if n > 0
        then assert $ (i === o)
        else return ()
  return (inPorts, outPort)

tinyExample :: GCM ()
tinyExample = do
  ([], a) <- cldNode (Just P) 0

  ([], b) <- cldNode (Just M) 0

  ([c1,c2], c) <- cldNode Nothing 2

  a --> c1
  b -+> c2
  output a "a"
  output b "b"
  output c "c"

tinyExample2 :: GCM ()
tinyExample2 = do
  ([], a) <- cldNode Nothing 0
  ([], b) <- cldNode Nothing 0

  ([c1,c2], c) <- cldNode (Just P) 2
  ([d1], d) <- cldNode (Just P) 1
  a -+> c1
  b --> c2
  b --> d1
  output a "a"
  output b "b"
  output c "c"
  output d "d"

drudzelHenrion :: GCM ()
drudzelHenrion = do
  ([a1], a ) <- cldNode (Just P) 1
  ([], b) <- cldNode Nothing 0
  ([c1], c) <- cldNode Nothing 1
  ([d1,d2], d) <- cldNode Nothing 2
  ([e1,e2], e) <- cldNode Nothing 2
  ([], f) <- cldNode Nothing 0
  b -+> a1
  b -+> c1
  b -+> d1
  c -+> d2
  d --> e1
  f --> e2

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

main :: IO ()
main = do
  --runCompare tinyExample
  --compileString drudzelHenrion
  runCompare drudzelHenrion

  --compileString tinyExample2
  runCompare tinyExample2
