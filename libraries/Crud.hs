module Crud (library) where

import Library

library :: Library
library = Library "crud"
    [ Item "rain" "Rain" "./data/img/rain.png" False $
         rain ::: "amount" # tInt .-> tGCM ("rainfall" # tPort tInt)

    , Item "simple pump" "Pump" "./data/img/pump.png" False $
       simplePump ::: "capacity" # tInt.-> tGCM (tPair ("inflow" # tPort tInt)
                                                       ("outflow" # tPort tInt))

    , Item "simple runoff area" "Runoff" "./data/img/runOffArea.png" False $
       simpleRunoffArea ::: "storage capacity" # tInt .-> tGCM (tTuple3 ("inflow" # tPort tInt)
                                                                        ("outlet" # tPort tInt)
                                                                        ("overflow" # tPort tInt))
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
