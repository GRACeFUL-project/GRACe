module Deliverable where

import Compile(runCompare)
import GCM     (GCM, output, component, Port, createPort, link, value, set)
import CP      (createLVar, assert, lit, (.>=), (.>), (===), (==>), inRange)
import Library (Library(Library), item, (#), TypedValue((:::)), tFloat,
                tPair, tTuple3, tPort, tGCM, (.->))

library :: Library
library = Library "crud"
    [ item "rain" $
        rain ::: "amount" #
          tFloat .-> tGCM          (tPort $ "rainfall" # tFloat)
    , item "pump" $
        pump ::: "capacity" #
          tFloat .-> tGCM (tPair   (tPort $ "inflow"   # tFloat)
                                   (tPort $ "outflow"  # tFloat))
    , item "runoff area" $
        runoffArea ::: "storage capacity" #
          tFloat .-> tGCM (tTuple3 (tPort $ "inflow"   # tFloat)
                                   (tPort $ "outlet"   # tFloat)
                                   (tPort $ "overflow" # tFloat))
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
    inflow  <- value inPort
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

example :: GCM ()
example = do
  (inflowP, outflowP) <- pump 5
  (inflowS, outletS, overflowS) <- runoffArea 5
  rainflow <- rain 10

  link inflowP outletS
  link inflowS rainflow

  output overflowS "Overflow"

main :: IO ()
main = do
  runCompare example
