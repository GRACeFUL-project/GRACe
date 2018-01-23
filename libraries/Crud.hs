module Crud (library
            , rain
            , simplePump
            ) where

import Library

type Annotation = String
rotF, rotT, inN, inS, outN, outS, outA :: Annotation
rotF  =  "rotation: false"
rotT  =  "rotation: true"
inN   =  "incomingType: none"
inS   =  "incomingType: single"
outN  =  "outgoingType: none"
outS  =  "outgoingType: single"
outA  =  "outgoingType: arbitrary"

library :: Library
library = Library "crud"
    [ Item "rain" ["description: Rain", "imgURL: ./data/img/rain.png",
                   "graphElement: nodal", "layer: domain"] $
         rain ::: "amount" # tInt .->
           tGCM (  rotF # inN # outA # "rainfall" # tPort tInt  )

    , Item "simple pump" ["description: Pump", "imgURL: ./data/img/pumpSimple.png",
                          "graphElement: relational", "layer: domain"] $
       simplePump ::: "capacity" # tInt .->
         tGCM (tPair  (rotT # inS # outN # "inflow"  # tPort tInt)
                      (rotT # inN # outS # "outflow" # tPort tInt)
              )

    , Item "simple runoff area" ["description: Runoff", "imgURL: ./data/img/runOffAreaSimple.png",
                                 "graphElement: nodal", "layer: domain"] $
       simpleRunoffArea ::: "storage capacity" # tInt .->
       tGCM (tTuple3  (rotT # inS # outN # "inflow"   # tPort tInt)
                      (rotF # inN # outS # "outlet"   # tPort tInt)
                      (rotT # inN # outS # "overflow" # tPort tInt))
    ]

rain :: Int -> GCM (Port Int)
rain amount = do
  port <- createPort
  set port amount
  return port

simplePump :: Int -> GCM (Port Int, Port Int)
simplePump maxCap = do
  inPort  <- createPort
  outPort <- createPort

  component $ do
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap)

  return (inPort, outPort)

simpleRunoffArea :: Int -> GCM (Port Int, Port Int, Port Int)
simpleRunoffArea cap = do
  inflow <- createPort
  outlet <- createPort
  overflow <- createPort

  component $ do
    currentStored <- createLVar

    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    sto <- value currentStored

    assert $ sto === inf - out - ovf
    assert $ sto `inRange` (0, lit cap)
    assert $ (ovf .> 0) ==> (sto === lit cap)
    assert $ ovf .>= 0

  return (inflow, outlet, overflow)
