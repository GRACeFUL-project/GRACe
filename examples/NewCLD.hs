module NewCLD where
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

cldArrow :: Sign -> GCM (Port Sign, Port Sign, Port Sign, Port Sign)
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
  return (inup,indown,outup,outdown)

cldLink :: Sign -> Port Sign -> Port Sign -> Port Sign -> Port Sign -> GCM ()
cldLink s pou pod qiu qid = do
  (iu, idn, ou, od) <- cldArrow s
  link pou iu
  link pod idn
  link qiu ou
  link qid od

-- TODO clarify what is what
cldNode :: Maybe Sign  -- Observed sign
        -> Int         -- No. of incoming arrows
        -> Int         -- No. of outgoing arrows
        -> GCM ( Port Sign  -- Sign of node
               ,[Port Sign] -- Flows up incoming arrows
               ,[Port Sign] -- Flows down incoming arrows
               ,[Port Sign] -- Flows up outgoing arrows
               ,[Port Sign] -- Flows down outgoing arrows
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
  return (valPort, inUps, inDowns, outUps, outDowns)

tinyExample :: GCM ()
tinyExample = do
  (a, [], [], [acu], [acd]) <- cldNode (Just P) 0 1

  (b, [], [], [bcu], [bcd]) <- cldNode (Just M) 0 1

  (c, [cau,cbu], [cad, cbd], [], []) <- cldNode Nothing 2 0

  cldLink M acu acd cau cad
  cldLink P bcu bcd cbu cbd

  output a "a"
  output b "b"
  output c "c"

tinyExample2 :: GCM ()
tinyExample2 = do
  (a, [], [], [acu], [acd]) <- cldNode Nothing 0 1
  (b, [], [], [bcu, bdu],[bcd, bdd]) <- cldNode Nothing 0 2

  (c,[cau, cbu], [cad, cbd], [], []) <- cldNode (Just P) 2 0
  (d, [dbu], [dbd], [], []) <- cldNode (Just P) 1 0
  cldLink P acu acd cau cad
  cldLink M bcu bcd cbu cbd
  cldLink M bdu bdd dbu dbd
  output a "a"
  output b "b"
  output c "c"
  output d "d"
-- Loops
positiveLoop :: GCM ()
positiveLoop = do
  (a, [abu], [abd], [ab2u], [ab2d]) <- cldNode (Just M) 1 1
  (b, [ba2u], [ba2d], [bau], [bad]) <- cldNode Nothing 1 1
  cldLink P abu abd bau bad
  cldLink P ba2u ba2d ab2u ab2d
  output a "a"
  output b "b"

posNegLoop :: GCM ()
posNegLoop = do
  (a, [abu], [abd], [ab2u], [ab2d]) <- cldNode (Just M) 1 1
  (b, [ba2u], [ba2d], [bau], [bad]) <- cldNode Nothing 1 1
  cldLink M abu abd bau bad
  cldLink M ba2u ba2d ab2u ab2d
  output a "a"
  output b "b"

-- | Negative feedback loop. The solver returns ambig for b.
negativeLoop :: GCM ()
negativeLoop = do
  (a, [abu], [abd], [ab2u], [ab2d]) <- cldNode (Just M) 1 1
  (b, [ba2u], [ba2d], [bau], [bad]) <- cldNode Nothing 1 1
  cldLink P abu abd bau bad
  cldLink M ba2u ba2d ab2u ab2d
  output a "a"
  output b "b"

-- | Example CLD from Druzdel & Henrion 1993
drudzelHenrion :: GCM ()
drudzelHenrion = do
  (a, [abu], [abd], [], []) <- cldNode (Just P) 1 0
  (b, [], [], [bau,bcu,bdu], [bad, bcd, bdd]) <- cldNode Nothing 0 3
  (c, [cbu], [cbd], [cdu], [cdd]) <- cldNode Nothing 1 1
  (d, [dbu, dcu], [dbd, dcd], [deu], [ded]) <- cldNode Nothing 2 1
  (e, [edu,efu], [edd, efd], [], [] ) <- cldNode Nothing 2 0
  (f, [], [], [feu], [fed]) <- cldNode Nothing 0 1
  cldLink P bau bad abu abd
  cldLink P bcu bcd cbu cbd
  cldLink P bdu bdd dbu dbd
  cldLink P cdu cdd dcu dcd
  cldLink M deu ded edu edd
  cldLink M feu fed efu efd

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

-- | Example CLD from p.51, Figure 3.6, of Van Kouwen’s Ph.D. thesis, The Quasta Approach
vanKouwen1 :: GCM ()
vanKouwen1 = do
  (a,[], [], [abu], [abd]) <- cldNode Nothing 0 1
  (b, [bau,beu,bcu],[bad, bed, bcd], [bgu,bdu], [bgd, bdd]) <- cldNode Nothing 3 2
  (c, [], [], [cbu,cdu],[cbd,cdd]) <- cldNode Nothing 0 2
  (d, [dcu, dbu], [dcd, dbd], [], []) <- cldNode Nothing 2 0
  (e, [], [], [ebu, efu], [ebd, efd]) <- cldNode Nothing 0 2
  (f, [feu], [fed], [fgu], [fgd]) <- cldNode Nothing 1 1
  (g, [gfu, gbu], [gfd, gbd], [], []) <- cldNode (Just P) 2 0
  --a -+> b2
  cldLink P abu abd bau bad
  --e -+> b1
  cldLink P ebu ebd beu bed
  --e -+> f1
  cldLink P efu efd feu fed
  --f -+> g1
  cldLink P fgu fgd gfu gfd
  --b -+> g2
  cldLink P bgu bgd gbu gbd
  --c --> b3
  cldLink M cbu cbd bcu bcd
  --c -+> d2
  cldLink P cdu cdd dcu dcd
  --b -+> d1
  cldLink P bdu bdd dbu dbd

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
  (a, [], [], [abu], [abd]) <- cldNode Nothing 0 1
  (b, [bau, beu, bcu], [bad, bed, bcd], [bgu, bdu], [bgd, bdd]) <- cldNode Nothing 3 2
  (c, [], [], [cbu, cdu], [cbd, cdd]) <- cldNode Nothing 0 2
  (d, [dbu,dcu],[dbd, dcd], [], []) <- cldNode (Just P) 2 0
  (e, [],[], [ebu, efu], [ebd, efd]) <- cldNode Nothing 0 2
  (f, [feu], [fed], [fgu], [fgd]) <- cldNode Nothing 1 1
  (g, [gbu,gfu],[gbd, gfd], [], []) <- cldNode Nothing 2 0

  --e -+> f1
  cldLink P efu efd feu fed
  --f -+> g1
  cldLink P fgu fgd gfu gfd
  --e -+> b1
  cldLink P ebu ebd beu bed
  --a -+> b2
  cldLink P abu abd bau bad
  --b -+> g2
  cldLink P bgu bgd gbu gbd
  --c --> b3
  cldLink M cbu cbd bcu bcd
  --b -+> d1
  cldLink P bdu bdd dbu dbd
  --c -+> d2
  cldLink P cdu cdd dcu dcd

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
  (a, [], [], [abu], [abd]) <- cldNode Nothing 0 1      -- Sea level rise
  (b, [bau,bdu], [bad,bdd], [bcu], [bcd]) <- cldNode Nothing 2 1 -- Risk of flooding
  (c, [cbu], [cbd], [cdu], [cdd]) <- cldNode Nothing 1 1     -- Flooding
  (d, [dcu, dfu], [dcd, dfd], [dbu, deu], [dbd,ded]) <- cldNode Nothing 2 2 -- Measures to prevent flooding
  (e, [edu],[edd],[], []) <- cldNode (Just P) 1 0   -- Ecology in coastal zone
  (f, [], [], [fdu], [fdd]) <- cldNode Nothing 0 1     -- Investments

  --a -+> b1
  cldLink P abu abd bau bad
  --b -+> c1
  cldLink P bcu bcd cbu cbd
  --c -+> d1
  cldLink P cdu cdd dcu dcd
  --d --> b2
  cldLink M dbu dbd bdu bdd
  --d --> e1
  cldLink M deu ded edu edd
  --f -+> d2
  cldLink P fdu fdd dfu dfd

  output a "a"
  output b "b"
  output c "c"
  output d "d"
  output e "e"
  output f "f"

-- | Example CLD from p.71, Figure 4.8, of Van Kouwen’s Ph.D. thesis, The Quasta Approach.
vanKouwen4 :: GCM ()
vanKouwen4 = do
  (a, [abu,acu], [abd,acd], [afu], [afd]) <- cldNode (Just M) 2 1 --Turbidity
  (b, [bgu,biu], [bgd,bid], [bau], [bad]) <- cldNode Nothing 2 1 -- Resuspended sedimentation
  (c, [ceu], [ced], [cau], [cad]) <- cldNode Nothing 1 1 -- Algae
  (d,[], [], [dfu], [dfd]) <- cldNode Nothing 0 1 -- Water depth
  (e,[ehu,efu,eku], [ehd,efd,ekd], [ecu], [ecd]) <- cldNode Nothing 3 1 -- Nutrients
  (f,[fau,fdu], [fad,fdd], [feu,fgu,fhu,fku], [fed,fgd,fhd,fkd]) <- cldNode Nothing 2 4 -- Vegetation
  (g,[gfu], [gfd], [gbu], [gbd]) <- cldNode Nothing 1 1 -- Waves
  (h,[hfu], [hfd], [heu], [hed]) <- cldNode Nothing 1 1 -- Allelopathic subs.
  (i,[], [], [ibu,iku], [ibd,ikd]) <- cldNode Nothing 0 2 -- Fish
  (k,[kfu,kiu], [kfd,kid], [keu], [ked]) <- cldNode Nothing 2 1 -- Zooplankton

  --b -+> a1
  cldLink P bau bad abu abd
  --c -+> a2
  cldLink P cau cad acu acd
  --g -+> b1
  cldLink P gbu gbd bgu bgd
  --i -+> b2
  cldLink P ibu ibd biu bid
  --e -+> c1
  cldLink P ecu ecd ceu ced
  --h --> e1
  cldLink M heu hed ehu ehd
  --f --> e2
  cldLink M feu fed efu efd
  --k --> e3
  cldLink M keu ked eku ekd
  --a --> f1
  cldLink M afu afd fau fad
  --d --> f2
  cldLink M dfu dfd fdu fdd
  --f --> g1
  cldLink M fgu fgd gfu gfd
  --f -+> h1
  cldLink P fhu fhd hfu hfd
  --f -+> k1
  cldLink P fku fkd kfu kfd
  --i --> k2
  cldLink M iku ikd kiu kid

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

tinyExample3 :: GCM ()
tinyExample3 = do
  (a, [], [], [acu], [acd]) <- cldNode (Just P) 0 1

  (b, [], [], [bcu], [bcd]) <- cldNode Nothing 0 1

  (c, [cau,cbu], [cad, cbd], [], []) <- cldNode Nothing 2 0

  cldLink M acu acd cau cad
  cldLink M bcu bcd cbu cbd

  output a "a"
  output b "b"
  output c "c"

main :: IO ()
main = do
  --runCompare tinierExample
  --compileString tinyExample
  putStrLn "tinyExample"
  runCompare tinyExample
  putStrLn "Drudzel Henrion"
  --compileString drudzelHenrion
  --compileMZ drudzelHenrion
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
  putStrLn "tinyExample3"
  --compileString tinyExample3
  runCompare tinyExample3
  putStrLn "tinyExample2"
  runCompare tinyExample2
  putStrLn "posLoop"
  runCompare positiveLoop
  putStrLn "posNegLoop"
  runCompare posNegLoop
  putStrLn "negLoop"
  runCompare negativeLoop
-}
