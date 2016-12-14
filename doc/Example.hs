
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
