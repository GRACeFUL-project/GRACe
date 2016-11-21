module Port (Port(..), ParameterPort(..), IsPort(..)) where

-- A port is just an address
data Port a = Port Int

-- A parameter port is a port with a default value
data ParameterPort a = ParameterPort a Int

-- Can I get a port ID
class IsPort p where
    portID :: p a -> Int

instance IsPort Port where
    portID (Port id) = id

instance IsPort ParameterPort where
    portID (ParameterPort _ id) = id
