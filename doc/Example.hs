
{- The first example in the presentation -}
data Pump = {capacity :: Param Flow,
             flow     :: Port Flow}

data Storage = {capacity :: Param Volume,
                inflow   :: Param Flow,
                overflow :: Param Flow}


pump :: Flow -> GCM Pump
pump = ...

storage :: Volume -> Maybe Pump -> GCM Storage
storage = ...

rain :: Flow -> GCM (Port Flow)
rain = ...

system = do
  rainPort <- rain 5
  pmp      <- pump 2
  store    <- storage 1 (Just pmp)

  link rainPort (inflow store)


{- Type signatures for example presentation -}

-- CP operations
assert :: CPExp Bool -> CP ()
value  :: Port a -> CP (CPExp a)

-- GCM operations
createPort   :: GCM (Port a)
createParam  :: a -> GCM (Param a)
createAction :: Param a 
             -> (CPExp Int -> CPExp a -> CPExp a) 
             -> GCM (Action a) 
createGoal   :: GCM Goal
link         :: Port a -> Port a -> GCM ()
component    :: CP () -> GCM ()

{- CP slide -}

pipe :: Float -> GCM (Port Float, Port Float, Param Float)
pipe defaultCapacity = do
  input    <- createPort
  output   <- createPort
  capacity <- createParam defaultCapacity
  
  component $ do
    inVal  <- value input
    outVal <- value output
    capVal <- value capacity
    
    assert $ inVal === outVal
    assert $ inVal `inRange` (0, capVal)
  
  return (input, output, capacity)

increasePipeCap :: Param Float -> GCM (Action Float)
increasePipeCap pipeCap = do
  increase <- createAction pipeCap 
                           (\inc def -> int2float inc + def)
  
  
  
