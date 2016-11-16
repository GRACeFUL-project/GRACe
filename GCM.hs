{-# LANGUAGE GADTs #-}
module GCM (GCM, link, createPort, component, fun) where 
import Port
import Program
import CP

-- A GRACeFUL concept map command
data GCMCommand a where
    Output     :: (CPType a, Show a) => Port a -> GCMCommand ()
    Link       :: (CPType a) => Port a -> Port a -> GCMCommand ()
    CreatePort :: (CPType a) => GCMCommand (Port a)
    Component  :: CP () -> GCMCommand ()

-- A GRACeFUL concept map
type GCM a = Program GCMCommand a

-- Syntactic sugars
output :: (CPType a, Show a) => Port a -> GCM ()
output = Instr . Output

createPort :: (CPType a) => GCM (Port a)
createPort = Instr CreatePort

component :: CP () -> GCM ()
component = Instr . Component

link :: (CPType a) => Port a -> Port a -> GCM ()
link p1 p2 = Instr (Link p1 p2)

-- Some derived operators
fun :: (CPType a, CPType b, Eq b) => (CPExp a -> CPExp b) -> GCM (Port a, Port b)
fun f = do
            pin  <- createPort
            pout <- createPort
            component $ do
                            i <- value pin
                            o <- value pout
                            assert $ o === f i
            return (pin, pout)
