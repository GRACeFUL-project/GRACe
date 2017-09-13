module Crud where
import Compile0
import GCM
import CP hiding (Proxy)
import GRACeGraph
import Library

-- example stuff
library :: Library
library = Library "Crud"
    [ Item "rain" "Rain" "./data/img/rain.png" $
         rain ::: "amount" # tFloat .-> tGCM ("rainfall" # tPort tFloat)
    ]

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port
