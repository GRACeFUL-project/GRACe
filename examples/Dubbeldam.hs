{-# LANGUAGE RecordWildCards #-}

module Main where

import GL
import Compile0

type Volume   = Int
type Cost     = Int
type Nuisance = Int
type Damage   = Int

-- | A pump component with capacity.
data Pump = Pump
  { flow :: Port Volume
  , cap  :: Param Volume
  }

-- | A storage component.
data Storage = Storage
  { inflow   :: Port Volume
  , outlet   :: Port Volume
  , overflow :: Port Volume
  -- TODO: Has no action
  }

-- | Something bad from a volume. TODO: What nonsense
data Bad a = Bad
  { volume :: Param Volume
  , cost   :: Port a
  } 

-- * Actions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- TODO: Several increaseStorage actions available

--  TODO
increaseStorage :: Volume -> GCM (Action Volume)
increaseStorage = undefined

-- TODO
increasePump :: Volume -> GCM (Action Volume)
increasePump = undefined

-- * Components
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | A rainfall event.
rainfall :: Volume -> GCM (Port Volume)
rainfall s = do
  p <- createPort
  set p s
  return p

-- | A pump with a fixed capacity.
pump :: Volume -> GCM Pump
pump c = do
  flow <- createPort
  cap  <- createParam c
  component $ do
    f <- value flow
    c <- value cap
    assert  $ f `inRange` (0, c)
  return Pump {..}

-- | The beige blob.
runOffArea :: Volume -> GCM Storage
runOffArea = storage

-- | A storage with a fixed capacity and a outlet.
-- TODO: Has no action.
storage :: Int -> GCM Storage
storage c = do
  inflow   <- createPort
  outlet   <- createPort
  overflow <- createPort
  
  component $ do
    inf <- value inflow
    pmp <- value outlet
    ovf <- value overflow
    assert $ ovf === max' 0 (inf - pmp - lit c)
    let sumFlow = ovf + pmp
    assert $ inf `inRange` (sumFlow, sumFlow + lit c)
 
  return Storage {..} 

-- | Flooding.
flooding :: GCM (Port Volume, Port Volume)
flooding = do
  runOff   <- createPort
  flooding <- createPort

  component $ do 
    ro  <- value runOff
    fl  <- value flooding
    assert $ f (ro, fl)

  return (runOff, flooding)
      where f = undefined -- TODO
  
-- | Sewer critical area.
sewerStorage :: Volume -> GCM Storage
sewerStorage = storage

-- * Damages
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

floodNuisance :: Nuisance -> GCM (Port Volume)
floodNuisance nuis = do
  flooding <- createPort

  component $ do
    flood <- value flooding
    assert $ f (flood) 

  return flooding
      where f = undefined -- TODO
  
floodDamage :: Damage -> GCM (Port Volume)
floodDamage dmg = do
  flooding <- createPort

  component $ do
    flood <- value flooding
    assert $ f (flood)

  return flooding
      where f = undefined

-- * Costs
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

storageCost :: GCM (Bad Cost) 
storageCost = do
  volume <- createParam (20 :: Int)
  cost   <- createPort

  component $ do
    vol <- value volume
    cst <- value cost
    assert $ f (vol, cost)

  return Bad {..}
    where f = undefined -- TODO

pumpCapCost :: GCM (Bad Cost)
pumpCapCost = do
  volume <- createParam (1 :: Int)
  cost   <- createPort

  component $ do
    vol <- value volume
    cst <- value cost
    assert $ f (vol, cost)

  return Bad {..}
    where f = undefined -- TODO
  
-- * Composition
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Dubbeldam pocket-case.
dubbeldam :: GCM ()
dubbeldam = do
  -- Rainfall Event
  rain <- rainfall 10

  -- Street, sewer and pump
  runOff <- runOffArea 0 -- Beige blob
  sewer  <- sewerStorage 0
  pump   <- pump 100

  -- Flooding Critical Area
  (floodIn, floodOut) <- flooding

  -- Damages and nuisances
  nuis <- floodNuisance 5
  dmg  <- floodDamage 3

  -- Actions
  -- TODO: incSewer etc would be one of 
  --  * Bioswale on parking
  --  * Bioswale instead of street
  --  * Green roofs
  incSewer <- increaseStorage 20 -- Increase sewer storage by 20
  incPump  <- increaseStorage 20 -- Increase pump capacity by 20

  -- Actions cost €
  costSewer <- storageCost     -- Cost for incSewer
  pumpCost  <- pumpCapCost     -- Cost for incPump

  -- Connections:
  link rain            (inflow runOff)    -- RainfallEvent -> Beige blob
  link (outlet runOff) floodIn            -- Beige blob    -> Flooding
  link (outlet runOff) (inflow sewer)     -- Beige blob    -> Sewer
  link (outlet sewer)  (flow pump)        -- Sewer         -> Pump
  link floodOut        dmg                -- Flooding      -> Flood-damage
  link floodOut        nuis               -- Flooding      -> Flood-nuisance
  

-- * IO
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main :: IO ()
main = runGCM dubbeldam

