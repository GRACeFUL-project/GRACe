{-# LANGUAGE GADTs #-}
module GCM (GCM, link, createPort, component, fun, compileGCM) where 
import Control.Monad.Writer
import Control.Monad.State.Lazy
import Port
import Program
import CP

-- A GRACeFUL concept map command
data GCMCommand a where
    Output     :: (CPType a) => Port a  -> GCMCommand ()
    CreatePort :: (CPType a) => Proxy a -> GCMCommand (Port a)
    Component  :: CP () -> GCMCommand ()

-- A GRACeFUL concept map
type GCM a = Program GCMCommand a

-- Syntactic sugars
output :: (CPType a, Show a) => Port a -> GCM ()
output = Instr . Output

createPort :: (CPType a) => GCM (Port a)
createPort = Instr (CreatePort Proxy)

component :: CP () -> GCM ()
component = Instr . Component

-- Some derived operators
link :: (CPType a, Eq a) => Port a -> Port a -> GCM ()
link p1 p2 = component $ do
                            v1 <- value p1
                            v2 <- value p2
                            assert $ v1 === v2

fun :: (CPType a, CPType b, Eq b) => (CPExp a -> CPExp b) -> GCM (Port a, Port b)
fun f = do
            pin  <- createPort
            pout <- createPort
            component $ do
                            i <- value pin
                            o <- value pout
                            assert $ o === f i
            return (pin, pout)

-- Compilation
data CompilationState = CompilationState {outputs :: [String], expressions :: [String], declarations :: [String], nextVarId :: Int}
type IntermMonad a = State CompilationState a

-- Translation
translateGCMCommand :: GCMCommand a -> IntermMonad a
translateGCMCommand (Output (Port x)) =
    do
        state <- get
        put $ state {outputs = ("v"++(show x)++"=\\(v"++(show x)++")\\n"):(outputs state)}
translateGCMCommand (CreatePort proxy) =
    do
        state <- get
        let vid = nextVarId state
            dec = "var " ++ (typeDec proxy) ++ ": v"++(show vid)++";"
            state' = state {nextVarId = vid+1, declarations = dec:(declarations state)}
        put state'
        return $ Port vid
translateGCMCommand (Component cp) =
    do
        state <- get
        let (_, exprs) = runWriter $ interpret translateCPCommands cp
            state'     = state {expressions = (expressions state) ++ exprs}
        put state'

-- Final compilation
compileGCM :: GCM a -> String
compileGCM gcm = stateToString $ (flip execState) (CompilationState [] [] [] 0) $ interpret translateGCMCommand gcm 
    where
        stateToString (CompilationState outs exprs declrs _) = unlines [unlines declrs, unlines exprs] ++ "\noutput "++(show outs) 
