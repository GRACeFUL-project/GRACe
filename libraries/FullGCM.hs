module FullGCM (library) where

import Library
import qualified CLDlib
import qualified Crud

library :: Library
library = insert is (combine "fullgcm" CLDlib.library Crud.library) where
  is =
    [ Item "pump" ["description: Pump", "imgURL: ./data/img/pump.png",
                   "graphElement: relational", "layer: domain"] $
       pump ::: "capacity" # tInt.->
       tGCM (tTuple3 ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                      "increase" # tPort tInt)
                     ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                      "inflow" # tPort tInt)
                     ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                      "outflow" # tPort tInt))

    , Item "runoff area" ["description: Runoff", "imgURL: ./data/img/runOffArea.png",
                          "graphElement: nodal", "layer: domain"] $
       runoffArea ::: "storage capacity" # tInt .->
       tGCM (tTuple4 ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                      "increase" # tPort tInt)
                     ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                      "inflow" # tPort tInt)
                     ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                      "outlet" # tPort tInt)
                     ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                      "overflow" # tPort tInt))

    , Item "sink" ["description: Sink", "imgURL: /dev/null",
                   "graphElement: nodal", "layer: domain"] $
        sink ::: tGCM ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                       "inflow" # tPort tInt)

    , Item "flooding" ["description: Flooding of square", "imgURL: /dev/null",
                       "graphElement: relational", "layer: domain"] $
        flooding ::: "numOut" # tInt .->
        tGCM (tPair ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                     "inflow" # tPort tInt)
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
             )
    , Item "increaseAction" ["description: Action to increase a parameter", "imgURL: /dev/null",
                             "graphElement: nodal", "layer: domain"] $
        increaseAction ::: "values" # tList tInt .-> "costs" # tList tInt .->
        tGCM (tPair ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tInt)
                    ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                     "cost"  # tPort tInt)
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
increaseAction = CLDlib.attachFunction

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
