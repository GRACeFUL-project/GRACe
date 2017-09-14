module Crud (library) where

import Library

library :: Library
library = Library "crud"
    [ Item "rain" "Rain" "./data/img/rain.png" $
         rain ::: "amount" # tFloat .-> tGCM ("rainfall" # tPort tFloat)

    , Item "pump" "Pump" "./data/img/pump.png"  $
        pump ::: "capacity" # tFloat.-> tGCM (tPair ("inflow" # tPort tFloat)
                                                    ("outflow" # tPort tFloat))

    , Item "runoff area" "Runoff" "./data/img/runOffArea.png" $
        runoffArea ::: "storage capacity" # tFloat .-> tGCM (tTuple3 ("inflow" # tPort tFloat)
                                                                     ("outlet" # tPort tFloat)
                                                                     ("overflow" # tPort tFloat))
    ]

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

pump :: Float -> GCM (Port Float, Port Float)
pump maxCap = do
  inPort  <- createPort
  outPort <- createPort

  component $ do
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap)

  return (inPort, outPort)

runoffArea :: Float -> GCM (Port Float, Port Float, Port Float)
runoffArea cap = do
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
