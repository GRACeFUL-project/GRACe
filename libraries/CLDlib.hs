module CLDlib (library, attachFunction, actionNode, funNode) where
import Library
import Compile
import Control.Monad

-- Missing urls to appropriate images
library :: Library
library = Library "cld"
  [ Item "node" ["description: Generic node", "imgURL: pathToNodeImage",
                 "graphElement: nodal", "layer: causal"] $
      cldNode ::: "obsSign" # (tMaybe tSign) .-> "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )

  , Item "edge" ["description: Causal relation", "imgURL: pathToArrowImage",
                 "graphElement: relational", "layer: causal"] $
      cldArrow ::: "sign" # tSign .->
      tGCM (tPair ("rotation: false" # "incomingType: multiple" # "outgoingType: none" #
                   "fromNode" # tPair (tPort tSign) (tPort tSign))
                  ("rotation: false" # "incomingType: none" # "outgoingType: multiple" #
                   "toNode" # tPair (tPort tSign) (tPort tSign)))

  , Item "budget" ["description: Set a maximum budget", "imgURL: /dev/null",
                   "layer: problem"] $
      budget ::: "numberOfPorts" # tInt .-> "maximumBudget" # tInt .->
                 tGCM (tPair
                       ("rotation: false" # "incomingType: arbitrary" # "outgoingType: none" #
                       "costs" # tList (tPort tInt))
                       ("rotation: false" # "incomingType: none" # "outgoingType: single" #
                       "totalCost" # tPort tInt))

  , Item "optimise" ["description: Optimise the sum of some ports", "imgURL: /dev/null",
                     "layer: problem"] $
      optimise ::: "numberOfPorts" # tInt .->
                   tGCM (tPair ("rotation: false" # "incomingType: arbitrary" # "outgoingType: none" #
                         "benefits" # tList (tPort tFloat))
                        ("rotation: false" # "incomingType: none" # "outgoingType: single" #
                         "totalBenefits" # tPort tFloat))

  , Item "evaluate" ["description: Evaluate benefits of possible values", "imgURL: /dev/null",
                     "layer: problem"] $
      evalBenefits ::: "values" # tList tSign .-> "weights" # tList tFloat .->
      tGCM (tPair ("rotation: false" # "incomingType: none" # "outgoingType: single" #
                   "atPort" # tPort tSign)
                  ("rotation: false" # "incomingType: none" # "outgoingType: single" #
                   "benefit" # tPort tFloat)
           )

  , Item "action" ["description: Node for action", "imgURL: /dev/null",
                   "graphElement: nodal", "layer: causal"] $
      actionNode ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                     "cost" # tPort tInt)
           )

  , Item "criterion" ["description: Node for criterion", "imgURL: /dev/null",
                      "graphElement: nodal", "layer: causal"] $
      funNode ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )

   , Item "stakeholder" ["description: Stakeholder", "imgURL: /dev/null",
                         "layer: problem"] $
     stakeHolder ::: "preferences" # tList (tList tSign) .-> "weights" # tList (tFloat) .->
     tGCM (tPair ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                  "criteria" # tList (tPort tSign))
                 ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                  "happiness" # (tPort tFloat)))

   , Item "critEdge" ["description: Edge to link a stakeholder to a criterion", "imgURL: /dev/null",
                      "graphElement: relational", "layer: problem"] $
     simpleEdge ::: tGCM (tPair
                          ("rotation: false" # "incomingType: single" # "outgoingType: none" #
                           "inPort" # tPort tSign)
                          ("rotation: false" # "incomingType: none" # "outgoingType: single" #
                           "outPort" # tPort tSign)
                         )

  ]

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

-- | General CLD node
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

-- | A simplified node whose value can be directly affected by attaching a function
funNode :: Int -> Int -> GCM (Port Sign,[(Port Sign, Port Sign)], [(Port Sign, Port Sign)])
funNode n m = do
  valPort <- createPort
  inUps <- mapM (\_ -> createPort) [1..n]    -- Flows up incoming arrows
  inDowns <- mapM (\_ -> createPort) [1..n]  -- Flows down incoming arrows
  outUps <- mapM (\_ -> createPort) [1..m]   -- Flows up outgoing arrows
  outDowns <- mapM (\_ -> createPort) [1..m] -- Flows down outgoing arrows
  inDown <- constructSum inDowns
  outUp <-  constructSum outUps
  mapM_ (\x -> link x valPort) inUps -- flows up incoming arrows
  mapM_ (\x -> link x valPort) outDowns -- flows down outgoing arrows
  component $ do
    v <- value valPort
    if n > 0 then do
      iD <- value inDown
      assert $ iD === v
      else
      if m > 0 then do
        oU <- value outUp
        assert $ (oU === v .|| v === lit Z)
        else return ()
  return (valPort, zip inUps inDowns, zip outUps outDowns)

-- | An edge to link two ports together
simpleEdge :: GCM (Port Sign, Port Sign)
simpleEdge = do
  pin <- createPort
  pout <- createPort
  link pin pout
  return (pin,pout)

actionNode :: [Sign] -> [Int] -> Int -> Int -> GCM (Port Sign,[(Port Sign, Port Sign)], [(Port Sign, Port Sign)], Port Int)
actionNode xs ys n m = do
  (valPort, costPort)  <- attachFunction xs ys
  inUps <- mapM (\_ -> createPort) [1..n]    -- Flows up incoming arrows
  inDowns <- mapM (\_ -> createPort) [1..n]  -- Flows down incoming arrows
  outUps <- mapM (\_ -> createPort) [1..m]   -- Flows up outgoing arrows
  outDowns <- mapM (\_ -> createPort) [1..m] -- Flows down outgoing arrows
  inDown <- constructSum inDowns
  outUp <-  constructSum outUps
  mapM_ (\x -> link x valPort) inUps -- flows up incoming arrows
  mapM_ (\x -> link x valPort) outDowns -- flows down outgoing arrows
  component $ do
    v <- value valPort
    if n > 0 then do
      iD <- value inDown
      assert $ iD === v
      else
      if m > 0 then do
        oU <- value outUp
        assert $ (oU === v .|| v === lit Z)
        else return ()
  return (valPort, zip inUps inDowns, zip outUps outDowns, costPort)

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

-- CLD actions, budget, and optimisation
attachFunction :: (CPType a, CPType b) => [b] -> [a] -> GCM (Port b, Port a)
attachFunction xs ys = do
  sd <- return $ zip xs ys
  s <- createPort
  d <- createPort
  component $ do
    vs <- value s
    vd <- value d
    assert $ foldl1 (.||) [ (vs === Lit sig) .&& (vd === Lit res) | (sig, res) <- sd ]
  return (s, d)

budget :: Int -> Int -> GCM ([Port Int], Port Int)
budget num cost = do
  total <- createPort
  ports <- mapM (const createPort) [1..num]
  component $ do
    vs <- mapM value ports
    t <- value total
    assert $ t === sum vs
    assert $ t .<= Lit cost
  return (ports, total)

optimise :: Int -> GCM ([Port Float], Port Float)
optimise num = do
  ports <- mapM (const createPort) [1..num]
  tot   <- createPort
  component $ do
    t <- value tot
    p <- mapM value ports
    assert $ t === sum p
  g <- createGoal
  link tot g
  return (ports, tot)

-- Takes a list of desired values and their weights and returns
-- the weighted sum of benefit for each possible value
stakeHolders :: [Sign] -> [Float] -> ([Sign], [Float])
stakeHolders xs ys = ([M,Z,P,Q], map (sH xs ys) [M,Z,P,Q]) where
  sH vs ws s = foldl (+) 0 (map (ev s) (zip vs ws))
  ev s (d,w) = if d == s then w else 0

-- Takes a list of desired values and their weights and returns
-- the weighted sum of benefit for each possible value
sh2 :: [Sign] -> [Float] -> [(Sign,Float)]
sh2 xs ys = zip [M,Z,P,Q] (map (sH xs ys) [M,Z,P,Q]) where
  sH vs ws s = foldl (+) 0 (map (ev s) (zip vs ws))
  ev s (d,w) = if d == s then w else 0

evalBenefits :: [Sign] -> [Float] -> GCM (Port Sign, Port Float)
evalBenefits x y = uncurry attachFunction $ stakeHolders x y

-- A stakeholder has a list of acceptable values and priority weight for
-- each criterion.
-- The ports contain the values of the criteria and the stakeholder's happiness.
stakeHolder :: [[Sign]] -> [Float] -> GCM ([Port Sign], Port Float)
stakeHolder preferences weights = do
  let n = length preferences
  crits <- mapM (const createPort) [1..n]
  happies <- mapM (const createPort) [1..n]
  happiness <- createPort
  let options = map (\(vs,w) -> sh2 vs (take (length vs) (repeat w))) (zip preferences weights)
  component $ do
    cs <- mapM value crits
    vh  <- value happiness
    hs <- mapM value happies
    mapM_ (\(c,h,ops)-> assert $
            foldl1 (.||) [ (c === Lit cr) .&& (h === Lit w) | (cr, w) <- ops ])
      (zip3 cs hs options)
    assert $ vh === sum hs
  return (crits, happiness)

-- A component for linking one port to many ports.
oneToMany :: CPType a => Int -> GCM (Port a, [Port a])
oneToMany num = do
  ports <- mapM (const createPort) [1..num]
  port <- createPort
  mapM_ (\p -> link p port) ports
  return (port, ports)

-- Useful functions for constructing examples
cldLink :: Sign -> (Port Sign, Port Sign) -> (Port Sign, Port Sign) -> GCM ()
cldLink s (pou, pod) (qiu, qid) = do
  ((iu, idn), (ou, od)) <- cldArrow s
  link pou iu
  link pod idn
  link qiu ou
  link qid od

port :: (Port a, b, c) -> Port a
port (p, _, _) = p

inc :: Int -> (a, [(Port Sign, Port Sign)], b) -> (Port Sign, Port Sign)
inc i (_, ps, _) = ps !! i

out :: Int -> (a, b, [(Port Sign, Port Sign)]) -> (Port Sign, Port Sign)
out i (_, _, ps) = ps !! i

acout :: Int -> (a, b, [(Port Sign, Port Sign)], c) -> (Port Sign, Port Sign)
acout i (_, _, ps, _) = ps !! i

acost :: (a,b,c, Port Int) -> Port Int
acost (_, _, _, x) = x

-- Some examples
simpleExample :: GCM ()
simpleExample = do
  let bud = 20

  -- value-cost pairs for actions
  (bioInput,        bioCost)  <- attachFunction [Z, P] [0,10]
  (pumpsInput,      pumpCost) <- attachFunction [Z, P] [0,5]

  -- value-benefit pairs for goals
  (greenSpaceInput, greenSpaceBenefit) <- evalBenefits [M,Z,P] [-5,0,7]
  (nuisanceInput,   nuisanceBenefit)    <- evalBenefits [M,Z,P] [5,0,-5]

  bioswale     <- funNode 0 2

  waterStorage <- cldNode Nothing 2 1
  pumps        <- funNode 0 1
  flooding     <- cldNode Nothing 1 1
  greenSpace   <- funNode 1 0
  nuisance     <- funNode 1 0

  (budgetPorts, total) <- budget 2 bud
  optimisePorts <- optimise 2

  zipWithM link [bioInput, pumpsInput, greenSpaceInput, nuisanceInput]
                [port bioswale, port pumps, port greenSpace, port nuisance]

  zipWithM link budgetPorts [bioCost, pumpCost]

  zipWithM link (fst optimisePorts) [greenSpaceBenefit, nuisanceBenefit]


  cldLink P (out 0 bioswale)     (inc 0 waterStorage)
  cldLink P (out 1 bioswale)     (inc 0 greenSpace)
  cldLink M (out 0 waterStorage) (inc 0 flooding)
  cldLink P (out 0 pumps)        (inc 1 waterStorage)
  cldLink P (out 0 flooding)     (inc 0 nuisance)

  output bioInput "bioswale action"
  output (port bioswale) "bioswale value"
  output bioCost "bioswale cost"
  output pumpsInput "pump action"
  output (port pumps) "pump value"
  output pumpCost "pump cost"
  output (port waterStorage) "water storage"
  output (port flooding) "flooding"
  output (port greenSpace) "greenSpace value"
  output greenSpaceInput "greenspace input"
  output greenSpaceBenefit "greenspace benefit"
  output (port nuisance) "nuisance value"
  output nuisanceInput "nuisance input"
  output nuisanceBenefit "nuisance benefit"

-- Example with stakeholders and different criteria
-- we can play around with changing the budget, costs, and preferences/priorities
-- and see how the results change
stakesExample :: GCM ()
stakesExample = do
  let bud = 20

  -- Nodes
  bioswale <- actionNode [Z,P] [0,10] 0 2
  fparking <- actionNode [Z,P] [0,15] 0 2

  waterStorage <- cldNode Nothing 2 1
  flooding     <- cldNode Nothing 1 1

  greenSpace   <- funNode 1 0
  nuisance     <- funNode 1 0
  pcap         <- funNode 1 0

  -- value-cost pairs for actions
  --(bioInput,        bioCost)  <- attachFunction [Z,P] [0,10]
  --(parkingInput, parkingCost) <- attachFunction [Z,P] [0,15]

  -- stakeholder 1 wants more green spaces and less nuisance, doesn't care about parking
  let s1 = ([P,M,Z], [0.67,0.33,0])
  -- stakeholder 2 wants less nuisance and more parking
  let s2 = ([Z,M,P],[0,0.67,0.33])

  -- value-benefit pairs for goals
  (greenSpaceInput, greenSpaceBenefit) <- evalBenefits [(fst s1) !! 0, (fst s2) !! 0] [(snd s1) !! 0, (snd s2) !! 0]
  (nuisanceInput,     nuisanceBenefit) <- evalBenefits [(fst s1) !! 1, (fst s2) !! 1] [(snd s1) !! 1, (snd s2) !! 1]
  (pcapInput,            pcapBenefit)  <- evalBenefits [(fst s1) !! 2, (fst s2) !! 2] [(snd s1) !! 2, (snd s2) !! 2]

  (budgetPorts, totalcost) <- budget 2 bud
  optimisePorts <- optimise 3

  zipWithM link [greenSpaceInput, nuisanceInput, pcapInput]
                [port greenSpace, port nuisance, port pcap]

  zipWithM link budgetPorts [acost bioswale, acost fparking]

  zipWithM link (fst optimisePorts) [greenSpaceBenefit, nuisanceBenefit, pcapBenefit]

  cldLink P (acout 0 bioswale)     (inc 0 waterStorage)
  cldLink P (acout 1 bioswale)     (inc 0 greenSpace)
  cldLink P (acout 0 fparking)     (inc 1 waterStorage)
  cldLink P (acout 1 fparking)     (inc 0 pcap)
  cldLink M (out 0 waterStorage) (inc 0 flooding)
  cldLink P (out 0 flooding)     (inc 0 nuisance)

  output (port greenSpace) "greenSpace value"
  output greenSpaceBenefit "greenspace benefit"
  output (port nuisance) "nuisance value"
  output nuisanceBenefit "nuisance benefit"
  output (port pcap) "pcap value"
  output pcapBenefit "pcap benefit"
  output (snd optimisePorts) "Total happiness"
  output totalcost "Total cost"

-- Same as above but with different stakeholder functions, should give identical results
stakesExample2 :: GCM ()
stakesExample2 = do
  let bud = 20

  -- Nodes
  bioswale <- actionNode [Z,P] [0,10] 0 2
  fparking <- actionNode [Z,P] [0,15] 0 2

  waterStorage <- cldNode Nothing 2 1
  flooding     <- cldNode Nothing 1 1

  greenSpace   <- funNode 1 0
  nuisance     <- funNode 1 0
  pcap         <- funNode 1 0

  -- value-cost pairs for actions
  --(bioInput,        bioCost)  <- attachFunction [Z,P] [0,10]
  --(parkingInput, parkingCost) <- attachFunction [Z,P] [0,15]

  -- stakeholder 1 wants more green spaces and less nuisance, doesn't care about parking
  --let s1 = ([P,M,Z], [0.67,0.33,0])
  ([g1,n1,p1], h1) <- stakeHolder [[P],[M],[P,M,Z]] [0.67, 0.33, 0]
  -- stakeholder 2 wants less nuisance and more parking
  --let s2 = ([Z,M,P],[0,0.67,0.33])
  ([g2,n2,p2], h2) <- stakeHolder [[P,M,Z],[M],[P]] [0,0.67,0.33]

  (budgetPorts, totalCost) <- budget 2 bud
  optimisePorts <- optimise 2

  zipWithM link [g1, n1, p1]
                [port greenSpace, port nuisance, port pcap]
  zipWithM link [g2, n2, p2]
                [port greenSpace, port nuisance, port pcap]

  zipWithM link budgetPorts [acost bioswale, acost fparking]

  zipWithM link (fst optimisePorts) [h1,h2]

  cldLink P (acout 0 bioswale)     (inc 0 waterStorage)
  cldLink P (acout 1 bioswale)     (inc 0 greenSpace)
  cldLink P (acout 0 fparking)     (inc 1 waterStorage)
  cldLink P (acout 1 fparking)     (inc 0 pcap)
  cldLink M (out 0 waterStorage) (inc 0 flooding)
  cldLink P (out 0 flooding)     (inc 0 nuisance)

  output (port greenSpace) "greenSpace value"
  --output greenSpaceBenefit "greenspace benefit"
  output (port nuisance) "nuisance value"
  --output nuisanceBenefit "nuisance benefit"
  output (port pcap) "pcap value"
  --output pcapBenefit "pcap benefit"
  output h1 "Happiness of stakeholder 1"
  output h2 "Happiness of stakeholder 2"
  output (snd optimisePorts) "Total happiness"
  output totalCost "Total cost"

-- Tiny example to translate to json
tinyExample :: GCM ()
tinyExample = do
  let bud = 20

  -- Nodes
  bioswale <- funNode 0 2

  waterStorage <- cldNode Nothing 1 1
  flooding     <- cldNode Nothing 1 1

  greenSpace   <- funNode 1 0
  nuisance     <- funNode 1 0

  -- value-cost pairs for actions
  (bioInput,        bioCost)  <- attachFunction [Z,P] [0,10]

  -- stakeholder 1 wants more green spaces and less nuisance
  let s1 = ([P,M], [1,1])
  -- stakeholder 2 wants less nuisance
  let s2 = ([Z,M], [0,2])

  -- value-benefit pairs for goals
  (greenSpaceInput, greenSpaceBenefit) <- evalBenefits [(fst s1) !! 0, (fst s2) !! 0] [(snd s1) !! 0, (snd s2) !! 0]
  (nuisanceInput,     nuisanceBenefit) <- evalBenefits [(fst s1) !! 1, (fst s2) !! 1] [(snd s1) !! 1, (snd s2) !! 1]

  (budgetPorts, totalcost) <- budget 1 bud
  optimisePorts <- optimise 2

  zipWithM link [bioInput, greenSpaceInput, nuisanceInput]
                [port bioswale, port greenSpace, port nuisance]

  zipWithM link budgetPorts [bioCost]

  zipWithM link (fst optimisePorts) [greenSpaceBenefit, nuisanceBenefit]

  cldLink P (out 0 bioswale)     (inc 0 waterStorage)
  cldLink P (out 1 bioswale)     (inc 0 greenSpace)
  cldLink M (out 0 waterStorage) (inc 0 flooding)
  cldLink P (out 0 flooding)     (inc 0 nuisance)

  output bioInput "bioswale action"
  output bioCost "bioswale cost"
  output (port greenSpace) "greenSpace value"
  output greenSpaceBenefit "greenspace benefit"
  output (port nuisance) "nuisance value"
  output nuisanceBenefit "nuisance benefit"

main :: IO ()
main = do
  runCompare False stakesExample
  runCompare False stakesExample2
  --runCompare tinyExample
  --compileString simpleExample
