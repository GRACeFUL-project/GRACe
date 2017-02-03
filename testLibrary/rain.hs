import GL
import Compile0

{-%
   "name"       : "rain",
 , "parameters" : [ {"name" : "amount",   "type" : "Float"} ]
 , "interface"  : [ {"name" : "rainfall", "type" : "Flow" } ]
 %-}
rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port
