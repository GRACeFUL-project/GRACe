module Port (Port, portID) where

-- A port is just an address
data Port a = Port Int | ParameterPort a Int

portID :: Port a -> Int
portID (Port i) = i
portID (ParameterPort a i) = i
