module RunoffArea where
import GL
import Compile0

{-%
   "name"       : "runoffArea"
 , "parameters" : [ {"name" : "storage capacity", "type" : "Float"} ]
 , "interface"  : [ {"name" : "inflow",           "type" : "Flow" }
                  , {"name" : "outlet",           "type" : "Flow" }
                  , {"name" : "overflow",         "type" : "Flow" }
                  ]
 %-}
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
