module FullGCM (library) where

import Library
import qualified CLDlib
import qualified Crud

library :: Library
library = insert is (combine "fullgcm" CLDlib.library Crud.library) where
  is =
    [ Item "pump" "Pump" "./data/img/pump.png" "Relational" $
       pump ::: "capacity" # tInt.-> tGCM (tTuple3 ("increase" # tPort tInt)
                                                   ("inflow" # tPort tInt)
                                                   ("outflow" # tPort tInt))
    , Item "runoff area" "Runoff" "./data/img/runOffArea.png" "Relational" $
       runoffArea ::: "storage capacity" # tInt .-> tGCM (tTuple4 ("increase" # tPort tInt)
                                                                    ("inflow" # tPort tInt)
                                                                    ("outlet" # tPort tInt)
                                                                    ("overflow" # tPort tInt))
    , Item "sink" "Sink" "/dev/null" "Nodal" $
        sink ::: tGCM ("inflow" # tPort tInt)
    , Item "flooding" "Flooding of square" "/dev/null" "Relational" $
        flooding ::: "numOut" # tInt .->
        tGCM (tPair ("inflow" # tPort tInt)
                    ("outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
             )
    , Item "increaseAction" "Action to increase a parameter" "/dev/null" "Nodal" $
        increaseAction ::: "values" # tList tInt .-> "costs" # tList tInt .->
        tGCM (tPair ("value" # tPort tInt)
                    ("cost"  # tPort tInt)
             )
    ]

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
increaseAction vals costs = CLDlib.attachFunction vals costs

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
