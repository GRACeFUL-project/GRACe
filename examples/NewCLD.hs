module NewCLD where
import GCM
import CP
import Compile
import Sign

cldArrow :: Sign -> GCM ((Port Sign, Port Sign), (Port Sign, Port Sign))
cldArrow s = do
  inup <- createPort
  indown <- createPort
  outup <- createPort
  outdown <- createPort
  component $ do
    iup <- value inup
    ido <- value indown
    oup <- value outup
    odo <- value outdown
    assert $ odo === (lit s) * ido -- flow in arrow direction
    assert $ iup === (lit s) * oup  -- flow in opposite direction
  return ((inup,indown),(outup,outdown))

cldLink :: Sign -> (Port Sign, Port Sign) -> (Port Sign, Port Sign) -> GCM ()
cldLink s (pou, pod) (qiu, qid) = do
  ((iu, idn), (ou, od)) <- cldArrow s
  link pou iu
  link pod idn
  link qiu ou
  link qid od

cldNode :: Maybe Sign  -- Observed sign
        -> Int         -- No. of incoming arrows
        -> Int         -- No. of outgoing arrows
        -> GCM ( Port Sign  -- Sign of node
               ,[(Port Sign, Port Sign)] -- Flows up and down incoming arrows
               ,[(Port Sign, Port Sign)] -- Flows up and down outgoing arrows
               )
cldNode obsSign n m = do
  valPort <- createPort
  inUps <- mapM (\_ -> createPort) [1..n]    -- Flows up incoming arrows
  inDowns <- mapM (\_ -> createPort) [1..n]  -- Flows down incoming arrows
  outUps <- mapM (\_ -> createPort) [1..m]   -- Flows up outgoing arrows
  outDowns <- mapM (\_ -> createPort) [1..m] -- Flows down outgoing arrows
  inDown <- constructSum inDowns
  outUp <-  constructSum outUps
  pOutUps <- partialSums outUps
  component $ do
    ius <- mapM value inUps
    ods <- mapM value outDowns
    pous <- mapM value pOutUps
    iD <- value inDown
    oU <- value outUp
    v <- value valPort
    case obsSign of
      Just s -> do
        assert $ (v === lit s) -- node value is observed value
        mapM_ (\x -> assert $ x === lit s) ius -- flows up incoming arrows
        mapM_ (\x -> assert $ x === lit s) ods -- flows down outgoing arrows
      Nothing -> do
        if n > 0 then do  -- there are incoming arrows
          mapM_ (\x -> assert $ x === oU) ius -- flows up incoming arrows are equal to
                                              -- sum of flows up outgoing arrows
          if m > 0 then do -- there are also outgoing arrows
            add valPort iD oU -- node value is sum of flows down incoming arrows
                              -- and flows up outgoing arrows
            mapM_ (\(x,y) -> add x y iD) (zip outDowns pous) -- flows down outgoing arrows
                                                             -- are the sum of flows up other
                                                             -- outgoing arrows and the sum of
                                                             -- flows down incoming arrows
          else assert $ v === iD -- node value is sum of flows down incoming arrows
        else do -- no incoming arrows
          if m > 0 then do
             assert $ v === oU  -- node value is sum of flows up outgoing arrows
             if m > 1 then mapM_ (\(x,y) -> assert $ x === y) (zip ods pous)
             else return ()
          else return ()
  return (valPort, zip inUps inDowns, zip outUps outDowns)


-- Helper functions to construct CLDs
----------------------------------------

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

partialSums :: [Port Sign] -> GCM [Port Sign]
partialSums xs = do
  let n = length xs
  res  <- mapM (\_ -> createPort) [1..n]
  mapM_ (\k -> pSum k xs res) [0..(n-1)]
  return res
  where
    pSum k ys zs = do
      before <- constructSum (take k ys)
      after <- constructSum (drop (k + 1) ys)
      b <- value before
      a <- value after
      component $ add (zs !! k) b a

tinyExample :: GCM ()
tinyExample = do
  (a, [], [ac]) <- cldNode (Just P) 0 1

  (b, [], [bc]) <- cldNode (Just M) 0 1

  (c, [ca,cb], []) <- cldNode Nothing 2 0

  cldLink M ac ca
  cldLink P bc cb

  output a "a"
  output b "b"
  output c "c"

tinyExample2 :: GCM ()
tinyExample2 = do
  (a, [], [ac]) <- cldNode Nothing 0 1
  (b, [], [bc, bd]) <- cldNode Nothing 0 2

  (c,[ca,cb], []) <- cldNode (Just P) 2 0
  (d, [db], []) <- cldNode (Just P) 1 0
  cldLink P ac ca
  cldLink M bc cb
  cldLink M bd db
  output a "a"
  output b "b"
  output c "c"
  output d "d"

-- Loops
positiveLoop :: GCM ()
positiveLoop = do
  (a, [ab], [ab2]) <- cldNode (Just M) 1 1
  (b, [ba2], [ba]) <- cldNode Nothing 1 1
  cldLink P ab ba
  cldLink P ba2 ab2
  output a "a"
  output b "b"

posNegLoop :: GCM ()
posNegLoop = do
  (a, [ab], [ab2]) <- cldNode (Just M) 1 1
  (b, [ba2], [ba]) <- cldNode Nothing 1 1
  cldLink M ab ba
  cldLink M ba2 ab2
  output a "a"
  output b "b"

-- | Negative feedback loop. The solver returns ambig for b.
negativeLoop :: GCM ()
negativeLoop = do
  (a, [ab], [ab2]) <- cldNode (Just M) 1 1
  (b, [ba2], [ba]) <- cldNode Nothing 1 1
  cldLink P ab ba
  cldLink M ba2 ab2
  output a "a"
  output b "b"

-- | Example CLD from Druzdel & Henrion 1993
drudzelHenrion :: GCM ()
drudzelHenrion = do
  (a, [ab], []) <- cldNode (Just P) 1 0
  (b, [], [ba,bc,bd]) <- cldNode Nothing 0 3
  (c, [cb], [cd]) <- cldNode Nothing 1 1
  (d, [db,dc], [de]) <- cldNode Nothing 2 1
  (e, [ed,ef], [] ) <- cldNode Nothing 2 0
  (f, [], [fe]) <- cldNode Nothing 0 1
  cldLink P ba ab
  cldLink P bc cb
  cldLink P bd db
  cldLink P cd dc
  cldLink M de ed
  cldLink M fe ef

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

-- | Example CLD from p.51, Figure 3.6, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
vanKouwen1 :: GCM ()
vanKouwen1 = do
  (a,[], [ab]) <- cldNode Nothing 0 1
  (b, [ba,be,bc], [bg, bd]) <- cldNode Nothing 3 2
  (c, [], [cb,cd]) <- cldNode Nothing 0 2
  (d, [dc, db], []) <- cldNode Nothing 2 0
  (e, [], [eb, ef]) <- cldNode Nothing 0 2
  (f, [fe], [fg]) <- cldNode Nothing 1 1
  (g, [gf, gb], []) <- cldNode (Just P) 2 0
  --a -+> b2
  cldLink P ab ba
  --e -+> b1
  cldLink P eb be
  --e -+> f1
  cldLink P ef fe
  --f -+> g1
  cldLink P fg gf
  --b -+> g2
  cldLink P bg gb
  --c --> b3
  cldLink M cb bc
  --c -+> d2
  cldLink P cd dc
  --b -+> d1
  cldLink P bd db

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
  (a, [], [ab]) <- cldNode Nothing 0 1
  (b, [ba, be, bc], [bg, bd]) <- cldNode Nothing 3 2
  (c, [], [cb, cd]) <- cldNode Nothing 0 2
  (d, [db,dc], []) <- cldNode (Just P) 2 0
  (e, [], [eb, ef]) <- cldNode Nothing 0 2
  (f, [fe], [fg]) <- cldNode Nothing 1 1
  (g, [gb,gf], []) <- cldNode Nothing 2 0

  --e -+> f1
  cldLink P ef fe
  --f -+> g1
  cldLink P fg gf
  --e -+> b1
  cldLink P eb be
  --a -+> b2
  cldLink P ab ba
  --b -+> g2
  cldLink P bg gb
  --c --> b3
  cldLink M cb bc
  --b -+> d1
  cldLink P bd db
  --c -+> d2
  cldLink P cd dc

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"
  output g "g"

-- | Example CLD from p.68, Figure 4.5, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
vanKouwen3 :: GCM ()
vanKouwen3 = do
  (a, [], [ab]) <- cldNode Nothing 0 1      -- Sea level rise
  (b, [ba,bd], [bc]) <- cldNode Nothing 2 1 -- Risk of flooding
  (c, [cb], [cd]) <- cldNode Nothing 1 1     -- Flooding
  (d, [dc,df], [db,de]) <- cldNode Nothing 2 2 -- Measures to prevent flooding
  (e, [ed], []) <- cldNode (Just P) 1 0   -- Ecology in coastal zone
  (f, [], [fd]) <- cldNode Nothing 0 1     -- Investments

  --a -+> b1
  cldLink P ab ba
  --b -+> c1
  cldLink P bc cb
  --c -+> d1
  cldLink P cd dc
  --d --> b2
  cldLink M db bd
  --d --> e1
  cldLink M de ed
  --f -+> d2
  cldLink P fd df

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

-- | Example CLD from p.71, Figure 4.8, of Van Kouwen’s Ph.D. thesis, The Quasta Approach.
vanKouwen4 :: GCM ()
vanKouwen4 = do
  (a, [ab,ac], [af]) <- cldNode (Just M) 2 1 --Turbidity
  (b, [bg,bi], [ba]) <- cldNode Nothing 2 1 -- Resuspended sedimentation
  (c, [ce], [ca]) <- cldNode Nothing 1 1 -- Algae
  (d, [], [df]) <- cldNode Nothing 0 1 -- Water depth
  (e, [eh,ef,ek], [ec]) <- cldNode Nothing 3 1 -- Nutrients
  (f, [fa,fd], [fe,fg,fh,fk]) <- cldNode Nothing 2 4 -- Vegetation
  (g, [gf], [gb]) <- cldNode Nothing 1 1 -- Waves
  (h, [hf], [he]) <- cldNode Nothing 1 1 -- Allelopathic subs.
  (i, [], [ib,ik]) <- cldNode Nothing 0 2 -- Fish
  (k, [kf,ki], [ke]) <- cldNode Nothing 2 1 -- Zooplankton

  --b -+> a1
  cldLink P ba ab
  --c -+> a2
  cldLink P ca ac
  --g -+> b1
  cldLink P gb bg
  --i -+> b2
  cldLink P ib bi
  --e -+> c1
  cldLink P ec ce
  --h --> e1
  cldLink M he eh
  --f --> e2
  cldLink M fe ef
  --k --> e3
  cldLink M ke ek
  --a --> f1
  cldLink M af fa
  --d --> f2
  cldLink M df fd
  --f --> g1
  cldLink M fg gf
  --f -+> h1
  cldLink P fh hf
  --f -+> k1
  cldLink P fk kf
  --i --> k2
  cldLink M ik ki

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
  putStrLn "tinyExample"
  runCompare tinyExample
  putStrLn "Drudzel Henrion"
  runCompare drudzelHenrion
  putStrLn "vk1"
  runCompare vanKouwen1
  putStrLn "vk2"
  runCompare vanKouwen2
  putStrLn "vk3"
  runCompare vanKouwen3
  putStrLn "vk4"
  runCompare vanKouwen4
  {-
  putStrLn "tinyExample2"
  runCompare tinyExample2
  putStrLn "posLoop"
  runCompare positiveLoop
  putStrLn "posNegLoop"
  runCompare posNegLoop
  putStrLn "negLoop"
  runCompare negativeLoop
-}
