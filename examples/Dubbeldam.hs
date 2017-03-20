{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleInstances      #-}

module Main where

import GL
import Compile0

type Volume   = Int
type Cost     = Int
type Nuisance = Int
type Damage   = Int

-- * Actions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- TODO: Several increaseStorage actions available

--  TODO: Docs
increaseStorage :: Param Volume -> GCM (Action Volume)
increaseStorage vol = createAction (+) vol

-- TODO: Docs
increasePump :: Param Volume -> GCM (Action Volume)
increasePump vol = createAction (+) vol

-- * Components
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | @'rainfall' s@ creates a rainfall event, parametrized on a measure of
-- rainfall.
rainfall :: Volume -> GCM (Port Volume)
rainfall s = do
  p <- createPort
  set p s
  return p

-- | A pump component with capacity.
data Pump = Pump
  { flow    :: Port Volume
  , pumpCap :: Param Volume
  }

-- | @'pump' c@ creates a pump parametrized on its maximum capacity @c@.
pump :: Volume -> GCM Pump
pump c = do
  flow    <- createPort
  pumpCap <- createParam c
  component $ do
    f <- value flow
    c <- value pumpCap
    assert  $ f `inRange` (0, c)
  return Pump {..}

-- | A storage component.
data Storage = Storage
  { inflow     :: Port Volume
  , outlet     :: Port Volume
  , overflow   :: Port Volume
  , storageCap :: Param Volume
  }

-- | @'storage' c@ creates a storage component parametrized on its capacity @c@.
storage :: Volume -> GCM Storage
storage c = do
  inflow     <- createPort
  outlet     <- createPort
  overflow   <- createPort
  storageCap <- createParam c

  component $ do
    inf <- value inflow
    pmp <- value outlet
    ovf <- value overflow
    c   <- value storageCap
    assert $ ovf === max' 0 (inf - pmp - c)
    let sumFlow = ovf + pmp
    assert $ inf `inRange` (sumFlow, sumFlow + c)

  return Storage {..}

-- | 'flooding' corresponds to the flooding node in the pocket case. If there is
-- overflow (i.e. something on the incoming port), then there is flooding.
flooding :: GCM (Port Volume, Port Bool)
flooding = do
  runOff   <- createPort
  flooding <- createPort

  linkBy (.>0) flooding runOff

  return (flooding, runOff)

-- | This is essentially the beige blob in the Dubbeldam pocket-case.
--
-- TODO: Document.
runOffArea :: Volume -> GCM Storage
runOffArea = storage

-- | Sewer critical area.
sewerStorage :: Volume -> GCM Storage
sewerStorage = storage

-- * Damages
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- TODO
floodNuisance :: Nuisance -> GCM (Port Bool)
floodNuisance nuis = do
  flooding <- createPort
  return flooding

-- TODO
floodDamage :: Damage -> GCM (Port Bool)
floodDamage dmg = do
  flooding <- createPort
  return flooding

-- * Costs
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | A cost associated with increasing storage.
storageCost :: Action Volume -> GCM (Port Cost)
storageCost a = do
  cost  <- createPort
  level <- taken a
  linkBy (\l -> 100 * l) level cost
  return cost

-- | A cost associated with increasing pump capacity.
pumpCapCost :: Action Volume -> GCM (Port Cost)
pumpCapCost = storageCost

-- | Restrict the space of possible actions.
restrict :: CPType a => Action a -> [Int] -> GCM ()
restrict a bs = do
  level <- taken a

  component $ do
    l <- value level
    assert $ foldl (.||) (lit True) [ l === lit b | b <- bs ]

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
  incSewer <- increaseStorage (storageCap sewer)
  incPump  <- increasePump    (pumpCap    pump)

  -- Actions cost €
  costSewer <- storageCost incSewer    -- Cost for incSewer
  pumpCost  <- pumpCapCost incPump     -- Cost for incPump

  -- Connections:
  link rain            (inflow runOff)    -- RainfallEvent -> Beige blob
  link (outlet sewer)  (flow   pump)      -- Sewer         -> Pump
  link (outlet runOff) (inflow sewer)     -- Beige blob    -> Sewer
  link (outlet runOff) floodIn            -- Beige blob    -> Flooding
  link floodOut        dmg                -- Flooding      -> Flood-damage
  link floodOut        nuis               -- Flooding      -> Flood-nuisance

  -- Outputs
  output (overflow sewer) "overflow sewer"
  output (overflow runOff) "overflow runOff"
  output (flow     pump)  "pump flow"

-- * IO
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

main :: IO ()
main = do msg <- runGCM dubbeldam
          putStrLn msg
