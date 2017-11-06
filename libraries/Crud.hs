module Crud (library) where

import Library

library :: Library
library = Library "crud"
    [ Item "rain" ["description: Rain", "imgURL: ./data/img/rain.png", "itemType: Nodal"] $
         rain ::: "amount" # tInt .->
         tGCM ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
               "rainfall" # tPort tInt)

    , Item "simple pump" ["description: Pump", "imgURL: ./data/img/pump.png", "itemType: Relational"] $
       simplePump ::: "capacity" # tInt.->
       tGCM (tPair ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                    "inflow" # tPort tInt)
                   ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                    "outflow" # tPort tInt))

    , Item "simple runoff area" ["description: Runoff", "imgURL: ./data/img/runOffArea.png", "itemType: Relational"] $
       simpleRunoffArea ::: "storage capacity" # tInt .->
       tGCM (tTuple3 ("rotation: true" # "incomingType: single" # "outgoingType: none" #
                      "inflow" # tPort tInt)
                     ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                      "outlet" # tPort tInt)
                     ("rotation: true" # "incomingType: none" # "outgoingType: single" #
                      "overflow" # tPort tInt))
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
