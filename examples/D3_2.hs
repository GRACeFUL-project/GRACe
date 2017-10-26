-- | The example from Fig. 3.2 on page 11 of D3.2
import GCM
import CP
import Sign
import Control.Monad
import Compile

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

-- Takes a list of desired values and their weights and returns
-- the weighted sum of benefit for each possible value
stakeHolders :: [Sign] -> [Float] -> ([Sign], [Float])
stakeHolders xs ys = ([M,Z,P,Q], map (sH xs ys) [M,Z,P,Q] )where
  sH vs ws s = foldl (+) 0 (map (ev s) (zip vs ws))
  ev s (d,w) = if d == s then w else 0

evalBenefits :: [Sign] -> [Float] -> GCM (Port Sign, Port Float)
evalBenefits x y = uncurry attachFunction $ stakeHolders x y

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

rain :: Int -> GCM (Port Int)
rain amount = do
  port <- createPort
  set port amount
  return port

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

-- The example from Fig. 3.2 on page 11 of D3.2
-- We can play around with changing the parameters (i.e. budget and action costs)
-- and see that the solver comes up with different outcomes.
hybridSFD :: GCM ()
hybridSFD = do
  -- rainFall
  r <- rain 10
  -- runOffArea_1
  (incR, inf, outf, ovf) <- runoffArea 5
  -- increaseStorageAction_1
  (incStorage, costStorage) <- increaseAction [0,1,2] [0,2,10]
  -- pump_1
  (incP, inP, outP) <- pump 2
  -- increasePumpCapacityAction_1
  (incPump, costPump) <- increaseAction [0,1,2,3] [0,5,8,11]
  -- sink_1
  s <- sink
  -- flooding_of_square
  (flowF, outFlood) <- flooding 2
  -- Flood nuisance
  nuisance     <- funNode 1 0
  -- Flood damage
  damage       <- funNode 1 0
  -- Budget for actions
  let bud = 10
  budgetPorts <- budget 2 bud
  zipWithM link budgetPorts [costStorage, costPump]

  -- stakeholders, s1 cares about nuisance, s2 about damage
  let s1 = ([Z,Z], [1,0])
  let s2 = ([Z,Z], [0,1])
  (nuisanceInput, nuisanceBenefit) <- evalBenefits [(fst s1) !! 0, (fst s2) !! 0] [(snd s1) !! 0, (snd s2) !! 0]
  (damageInput, damageBenefit)  <- evalBenefits [(fst s1) !! 1, (fst s2) !! 1] [(snd s1) !! 1, (snd s2) !! 1]
  zipWithM link [nuisanceInput, damageInput] [port nuisance, port damage]

  --optimization
  optimisePorts <- optimise 2
  zipWithM link optimisePorts [nuisanceBenefit, damageBenefit]

  link incR incStorage
  link incP incPump
  link r inf
  link outf inP
  link outP s
  link flowF ovf
  cldLink P (outFlood !! 0) (inc 0 nuisance)
  cldLink P (outFlood !! 1) (inc 0 damage)

  -- Output solution
  output (incStorage) "storage increase"
  output (costStorage) "storage cost"
  output (incPump)     "pump increase"
  output (costPump) "pump cost"
  output (port nuisance) "nuisance"
  output (port damage)   "damage"


main :: IO ()
main = do
  runCompare hybridSFD
