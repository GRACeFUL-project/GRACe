module FullGCM (library) where

import Library

library :: Library
library = Library "fullgcm"
  [ Item "node" "Generic node" "pathToNodeImage" False $
      cldNode ::: "obsSign" # (tMaybe tSign) .-> "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("value" # tPort tSign)
                    ("incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )

  , Item "edge" "Causal relation" "pathToArrowImage" True $
      cldArrow ::: "sign" # tSign .-> tGCM (tPair ("fromNode" # tPair (tPort tSign) (tPort tSign))
                                                  ("toNode"   # tPair (tPort tSign) (tPort tSign))
                                         )

  , Item "budget" "Set a maximum budget" "/dev/null" False $
      budget ::: "numberOfPorts" # tInt .-> "maximumBudget" # tInt .->
                 tGCM ("costs" # tList (tPort tInt))

  , Item "optimise" "Optimise the sum of some ports" "/dev/null" False $
      optimise ::: "numberOfPorts" # tInt .->
                   tGCM ("benefits" # tList (tPort tFloat))

  , Item "evaluate" "Evaluate benefits of possible values" "/dev/null" False $
      evalBenefits ::: "values" # tList tSign .-> "weights" # tList tFloat .->
      tGCM (tPair ("atPort" # tPort tSign)
                  ("benefit" # tPort tFloat)
           )

  , Item "action" "CLD action node" "/dev/null" False $
      actionNode ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("value" # tPort tSign)
                    ("incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("cost" # tPort tInt)
           )

  , Item "criterion" "Node for criterion" "/dev/null" False $
      funNode ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("value" # tPort tSign)
                    ("incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )

  , Item "rain" "Rain" "./data/img/rain.png" False $
       rain ::: "amount" # tInt .-> tGCM ("rainfall" # tPort tInt)

  , Item "pump" "Pump" "./data/img/pump.png" False $
       pump ::: "capacity" # tInt.-> tGCM (tTuple3 ("increase" # tPort tInt)
                                                   ("inflow" # tPort tInt)
                                                   ("outflow" # tPort tInt))

  , Item "runoff area" "Runoff" "./data/img/runOffArea.png" False $
       runoffArea ::: "storage capacity" # tInt .-> tGCM (tTuple4 ("increase" # tPort tInt)
                                                                    ("inflow" # tPort tInt)
                                                                    ("outlet" # tPort tInt)
                                                                    ("overflow" # tPort tInt))
  , Item "sink" "Sink" "/dev/null" False $
      sink ::: tGCM ("inflow" # tPort tInt)
  , Item "flooding" "Flooding of square" "/dev/null" False $
      flooding ::: "numOut" # tInt .->
      tGCM (tPair ("inflow" # tPort tInt)
                  ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "increaseAction" "Action to increase a parameter" "/dev/null" False $
      increaseAction ::: "values" # tList tInt .-> "costs" # tList tInt .->
      tGCM (tPair ("value" # tPort tInt)
                  ("cost"  # tPort tInt)
           )
    ]

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

attachFunction :: (CPType a, CPType b) => [a] -> [b] -> GCM (Port a, Port b)
attachFunction xs ys = do
  sd <- return $ zip xs ys
  s <- createPort
  d <- createPort
  component $ do
    vs <- value s
    vd <- value d
    assert $ foldl1 (.||) [ (vs === Lit sig) .&& (vd === Lit res) | (sig, res) <- sd ]
  return (s, d)

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

rain :: Int -> GCM (Port Int)
rain amount = do
  port <- createPort
  set port amount
  return port

runoffArea :: Int -> GCM (Port Int, Port Int, Port Int, Port Int)
runoffArea cap = do
  increase <- createPort
  inflow <- createPort
  outlet <- createPort
  overflow <- createPort

  component $ do
    currentStored <- createLVar

    incr <- value increase
    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    sto <- value currentStored

    assert $ sto === inf - out - ovf
    assert $ sto `inRange` (0, lit cap + incr)
    assert $ (ovf .> 0) ==> (sto === lit cap + incr)
    assert $ ovf .>= 0

  return (increase, inflow, outlet, overflow)

pump :: Int -> GCM (Port Int, Port Int, Port Int)
pump maxCap = do
  increase <- createPort
  inPort  <- createPort
  outPort <- createPort
  component $ do
    incr <- value increase
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap + incr)
  return (increase, inPort, outPort)

sink :: GCM (Port Int)
sink = do
  port <- createPort
  return port

increaseAction :: [Int] -> [Int] -> GCM (Port Int, Port Int)
increaseAction vals costs = attachFunction vals costs

flooding :: Int -> GCM (Port Int, [(Port Sign, Port Sign)])
flooding numOut = do
  flow       <- createPort
  floodPorts <- mapM (const createPort) [1..numOut]
  upPorts    <- mapM (const createPort) [1..numOut]
  component $ do
    f <- value flow
    vs <- mapM value floodPorts
    mapM_ (\x -> assert $ ((x === Lit P .&& f .> 0) .|| (x === Lit Z .&& f .<= 0))) vs
  return (flow, zip upPorts floodPorts)

budget :: Int -> Int -> GCM [Port Int]
budget num cost = do
  ports <- mapM (const createPort) [1..num]
  component $ do
    vs <- mapM value ports
    assert $ sum vs .<= Lit cost
  return ports

optimise :: Int -> GCM [Port Float]
optimise num = do
  ports <- mapM (const createPort) [1..num]
  tot   <- createPort
  component $ do
    t <- value tot
    p <- mapM value ports
    assert $ t === sum p
  g <- createGoal
  link tot g
  return ports

stakeHolders :: [Sign] -> [Float] -> ([Sign], [Float])
stakeHolders xs ys = ([M,Z,P,Q], map (sH xs ys) [M,Z,P,Q] )where
  sH vs ws s = foldl (+) 0 (map (ev s) (zip vs ws))
  ev s (d,w) = if d == s then w else 0

evalBenefits :: [Sign] -> [Float] -> GCM (Port Sign, Port Float)
evalBenefits x y = uncurry attachFunction $ stakeHolders x y
