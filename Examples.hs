module Examples where
import CP
import Port
import GCM

-- A pump as a GCM component
pump :: Float ->  GCM (Port Float)
pump k =
    do
        p <- createPort
        component $ do
                     inflow <- value p
                     assert $  (inflow .<= lit k) .&& (0 .<= inflow)
        return p

-- Rain as a GCM component
rain :: Float -> GCM (Port Float)
rain k =
    do
        p <- createPort
        component $ do
                      outflow <- value p
                      assert  $  outflow === lit k
        return p

-- A pipe as a GCM component
pipe :: Float -> GCM (Port Float, Port Float)
pipe k =
    do
        ip <- createPort
        op <- createPort
        component $ do
                      inflow  <- value ip
                      outflow <- value op
                      assert  $ (inflow .<= lit k) .&& (0 .<= inflow) 
                      assert  $ outflow === inflow
        return (ip, op)

-- Storage as a GCM component
storage :: Float -> GCM (Port Float, Port Float)
storage k = fun (\inflow -> min' 0 (inflow - lit k))
