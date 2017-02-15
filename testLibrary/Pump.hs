module Pump where
import GL
import Compile0

{-%
   "name"       : "pump"
 , "parameters" : [ {"name" : "capacity", "type" : "Float"} ]
 , "interface"  : [ {"name" : "inflow",   "type" : "Flow" }
                  , {"name" : "outflow",  "type" : "Flow" }
                  ]
 %-}
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
