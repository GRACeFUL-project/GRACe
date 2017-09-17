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

-- Loops
positiveLoop :: GCM ()
positiveLoop = do
  ([a1],a) <- cldNode (Just M) 1
  ([b1],b) <- cldNode Nothing 1
  a -+> b1
  b -+> a1
  output a "a"
  output b "b"

posNegLoop :: GCM ()
posNegLoop = do
  ([a1],a) <- cldNode (Just M) 1
  ([b1],b) <- cldNode Nothing 1
  a --> b1
  b --> a1
  output a "a"
  output b "b"

-- | Negative feedback loop. The solver returns UNSAT.
negativeLoop :: GCM ()
negativeLoop = do
  ([a1],a) <- cldNode (Just M) 1
  ([b1],b) <- cldNode Nothing 1
  a -+> b1
  b --> a1
  output a "a"
  output b "b"

-- | Example CLD from Druzdel & Henrion 1993
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

-- | Example CLD from p.51, Figure 3.6, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
vanKouwen1 :: GCM ()
vanKouwen1 = do
  ([], a) <- cldNode Nothing 0
  ([b1,b2,b3], b) <- cldNode Nothing 3
  ([], c) <- cldNode Nothing 0
  ([d1,d2], d) <- cldNode Nothing 2
  ([], e) <- cldNode Nothing 0
  ([f1], f) <- cldNode Nothing 1
  ([g1,g2],g) <- cldNode (Just P) 2
  a -+> b2
  e -+> b1
  e -+> f1
  f -+> g1
  b -+> g2
  c --> b3
  c -+> d2
  b -+> d1

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"
  output g "g"

-- | Example CLD from p.52, Figure 3.8, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
vanKouwen2 :: GCM ()
vanKouwen2 = do
  ([],a) <- cldNode Nothing 0
  ([b1,b2,b3],b) <- cldNode Nothing 3
  ([],c) <- cldNode Nothing 0
  ([d1,d2],d) <- cldNode (Just P) 2
  ([],e) <- cldNode Nothing 0
  ([f1],f) <- cldNode Nothing 1
  ([g1,g2],g) <- cldNode Nothing 2

  e -+> f1
  f -+> g1
  e -+> b1
  a -+> b2
  b -+> g2
  c --> b3
  b -+> d1
  c -+> d2

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"
  output g "g"

-- | Example CLD from p.68, Figure 4.5, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
--   Contains negative feedback loop, solver says UNSAT.
vanKouwen3 :: GCM ()
vanKouwen3 = do
  ([],a) <- cldNode Nothing 0       -- Sea level rise
  ([b1,b2],b) <- cldNode Nothing 2  -- Risk of flooding
  ([c1],c) <- cldNode Nothing 1     -- Flooding
  ([d1,d2], d) <- cldNode Nothing 2 -- Measures to prevent flooding
  ([e1],e) <- cldNode (Just P) 1    -- Ecology in coastal zone
  ([], f) <- cldNode Nothing 0      -- Investments

  a -+> b1
  b -+> c1
  c -+> d1
  d --> b2
  d --> e1
  f -+> d2

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

-- | Example CLD from p.71, Figure 4.8, of Van Kouwen’s Ph.D. thesis, The Quasta Approach.
vanKouwen4 :: GCM ()
vanKouwen4 = do
  ([a1,a2],a) <- cldNode (Just M) 2 --Turbidity
  ([b1,b2],b) <- cldNode Nothing 2 -- Resuspended sedimentation
  ([c1],c) <- cldNode Nothing 1 -- Algae
  ([],d) <- cldNode Nothing 0 -- Water depth
  ([e1,e2,e3],e) <- cldNode Nothing 3 -- Nutrients
  ([f1,f2],f) <- cldNode Nothing 2 -- Vegetation
  ([g1],g) <- cldNode Nothing 1 -- Waves
  ([h1],h) <- cldNode Nothing 1 -- Allelopathic subs.
  ([],i) <- cldNode Nothing 0 -- Fish
  ([k1,k2],k) <- cldNode Nothing 2 -- Zooplankton

  b -+> a1
  c -+> a2
  g -+> b1
  i -+> b2
  e -+> c1
  h --> e1
  f --> e2
  k --> e3
  a --> f1
  d --> f2
  f --> g1
  f -+> h1
  f -+> k1
  i --> k2

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"
  output g "g"
  output h "h"
  output i "i"
  output k "k"

main :: IO ()
main = do
  --runCompare tinyExample
  --compileString drudzelHenrion
  --runCompare drudzelHenrion
  runCompare vanKouwen4
  --runCompare tinyExample2
  --runCompare positiveLoop
  --runCompare posNegLoop
  runCompare negativeLoop
