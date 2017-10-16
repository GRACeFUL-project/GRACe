module NewCLDlib (library) where
import Library
import Compile
import Control.Monad

-- Missing urls to appropriate images
library :: Library
library = Library "cld"
  [ Item "node" "Node" "pathToNodeImage" False $
      cldNode ::: "obsSign" # (tMaybe tSign) .-> "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("value" # tPort tSign)
                    ("incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )

  , Item "edge" "Causal relation" "pathToArrowImage" True $
      cldArrow ::: "sign" # tSign .-> tGCM (tPair ("fromNode" # tPair (tPort tSign) (tPort tSign))
                                                  ("toNode"   # tPair (tPort tSign) (tPort tSign))
                                         )

  , Item "attachFunction" "Attach a function to a port" "/dev/null" False $
      attachFunction ::: "mapping" # tList (tPair tSign tInt) .-> tGCM (tPair ("atPort" # tPort tSign)
                                                                              ("funValue" # tPort tInt)
                                                                       )

  , Item "budget" "Set a maximum budget" "/dev/null" False $
      budget ::: "numberOfPorts" # tInt .->
                 "maximumBudget" # tInt .->
                 tGCM (tList (tPort tInt))

  , Item "optimise" "Optimise the sum of some ports" "/dev/null" False $
      optimise ::: "numberOfPorts" # tInt .->
                   tGCM (tList (tPort tInt))
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

-- Useful functions for constructing examples
port :: (Port a, b, c) -> Port a
port (p, _, _) = p

inc :: Int -> (a, [(Port Sign, Port Sign)], b) -> (Port Sign, Port Sign)
inc i (_, ps, _) = ps !! i

out :: Int -> (a, b, [(Port Sign, Port Sign)]) -> (Port Sign, Port Sign)
out i (_, _, ps) = ps !! i

-- CLD actions, budget, and optimisation

attachFunction :: [(Sign, Int)] -> GCM (Port Sign, Port Int)
attachFunction sd = do
  s <- createPort
  d <- createPort
  component $ do
    vs <- value s
    vd <- value d
    assert $ foldl (.||) ((1 :: CPExp Int) === 0) [ (vs === Lit sig) .&& (vd === Lit res) | (sig, res) <- sd ]
  return (s, d)

budget :: Int -> Int -> GCM [Port Int]
budget num cost = do
  ports <- mapM (const createPort) [1..num]
  component $ do
    vs <- mapM value ports
    assert $ sum vs .<= Lit cost
  return ports

optimise :: Int -> GCM [Port Int]
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

-- Some examples
simpleExample :: GCM ()
simpleExample = do
  let bud = 10

  bioswale     <- cldNode Nothing 0 2
  waterStorage <- cldNode Nothing 2 1
  pumps        <- cldNode Nothing 0 1
  flooding     <- cldNode Nothing 1 1
  greenSpace   <- cldNode Nothing 1 0
  nuisance     <- cldNode Nothing 1 0

  -- value-cost pairs for actions
  (bioInput,        bioCost)  <- attachFunction [ (Z, 0), (P, 10) ]
  (pumpsInput,      pumpCost) <- attachFunction [ (Z, 0), (P, 5)  ]
  (greenSpaceInput, greenSpaceBenefit) <- attachFunction [ (M, -5), (Z, 0), (P, 5) ]
  (nuisanceInput,   nuisanceBenefit)    <- attachFunction [ (M, 5), (Z, 0), (P, -5) ]

  budgetPorts <- budget 2 bud
  optimisePorts <- optimise 2

  zipWithM link budgetPorts [bioCost, pumpCost]

  zipWithM link optimisePorts [greenSpaceBenefit, nuisanceBenefit]

  zipWithM link [bioInput, pumpsInput, greenSpaceInput, nuisanceInput]
                [port bioswale, port pumps, port greenSpace, port nuisance]

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
  return ()

--main :: IO ()
--main = runCompare simpleExample
