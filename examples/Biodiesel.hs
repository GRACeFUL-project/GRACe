{-# LANGUAGE RecordWildCards       #-}

module Main where

import Compile0 (runGCM)
import GCM      ( GCM, output, component
                , Param, createParam
                , Goal, createGoal
                , Action, createAction, taken
                , Port, createPort, link, linkBy, value, set, fun
                )
import CP       ( CPType, assert, lit, max', (===), (.<=), inRange, CPExp)

{-- * TODO: Needs a problem description at some point -}

-- * Vegetable oil manufacturing
-------------------------------------------------------------------------------

-- | There are three crops which provide us with vegetable oil:
--
-- Soybeans, sunflower seeds and cotton seeds.
data Crop = Soy | Sunflower | Cotton

type Area  = Float
type Yield = Float
type Water = Float
type Oil   = Float

-- | Crop yield/water demand table:
--
--   Crop            Yield [t/ha]   Water demand [Ml/ha]  Oil content [l/kg]
--   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   Soybeans        2.5            5.0                   0.178
--   Sunflower seeds 1.5            4.2                   0.216
--   Cotton seeds    0.8            1.0                   0.433
--
-- Note the units.
cropTable :: Crop -> (Yield, Water, Oil)
cropTable Soy       = (2.5, 5.0, 0.178)
cropTable Sunflower = (1.5, 4.2, 0.216)
cropTable Cotton    = (0.8, 1.0, 0.433)

-- | GCM component for a single crop.
--
-- The component is parametrized on the crop and computes the oil yield (in
-- litres) and water consumption, given that we grow so-and-so many ha of this
-- crop.
--
-- Makes use of the tabulated data from 'cropTable'.
cropToOil :: Crop -> GCM (Port Area, Port Oil, Port Water)
cropToOil crop = do
  -- Declare ports.
  areaPort  <- createPort
  oilPort   <- createPort
  waterPort <- createPort

  -- Constrain the values at the ports.
  component $ do
    areaValue  <- value areaPort
    oilValue   <- value oilPort
    waterValue <- value waterPort

    -- Get tabulated values for crop.
    let (y, w, o) = cropTable crop

    -- For some reason, yield is given as tonnes and oil in litres per kg.
    assert $ oilValue   === 1000 * lit y * lit o * areaValue
    assert $ waterValue === lit w * areaValue

  return (areaPort, oilPort, waterPort)

-- | GCM component for optimizing crop production to provide the maximum benefit
-- for our Biodiesel production. The following quantities are available:
--
--   Farmland: 1,600 ha
--   Water:    5,000 Ml
--
-- (Although implicit, this just amounts to maximizing vegetable oil yield given
-- the available farmland and water.)
oilProduction :: GCM (Port Oil)
oilProduction = do
  let farmland = 1600
      water    = 5000

  -- Port for total vegetable oil yield.
  oilYieldPort <- createPort

  -- Get the relevant ports for all crops.
  cropPorts <- mapM cropToOil [Soy, Sunflower, Cotton]

  -- 1. The total area of crops is non-negative and is bounded by the available
  --    farmland.
  -- 2. The total amount of water used is non-negative and is bounded by the
  --    available water reservoir.
  component $ do
    -- Values for crops: (area, water, oil)
    areas  <- sequence [ value ap | (ap, _, _) <- cropPorts ]
    waters <- sequence [ value wp | (_, wp, _) <- cropPorts ]
    oils   <- sequence [ value op | (_, _, op) <- cropPorts ]
    yield  <- value oilYieldPort

    assert $ sum areas `inRange` (0, lit farmland)
    assert $ sum waters `inRange` (0, lit water)
    assert $ sum oils === yield

  return oilYieldPort

-- * Biodiesel manufacturing
-------------------------------------------------------------------------------

type Methanol = Float
type Diesel   = Float
type Petrol   = Float
type Tax      = Float
type Euro     = Float

-- | There are three blends; B5, B30 and B100 (denoting percentage of biodiesel
-- content).
data Blend = B5 | B30 | B100

-- | Biodiesel blend table:
--
--   Blend   Biodiesel [%]  Price [EUR/l]  Tax [%]
--   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   B5      5              1.42           20
--   B30     30             1.27           5
--   B100    100            1.15           0
blendTable :: Blend -> (Float, Euro, Tax)
blendTable B5   = (0.05, 1.42, 0.20)
blendTable B30  = (0.30, 1.27, 0.05)
blendTable B100 = (1.00, 1.15, 0.00)

-- TODO We just need a lot of stuff to create biodiesel.
--      Perhaps profits can be moved elsewhere.
data BlendRec =
  BlendRec
    { dieselPort   :: Port Diesel
    , petrolPort   :: Port Petrol
    , methanolPort :: Port Methanol
    , oilPort      :: Port Oil
    , profitPort   :: Port Euro
    }

-- | GCM component for a single biodiesel blend.
--
-- Biodiesel is manufactured by blending vegetable oil with methanol according
-- to the folowing formula:
--
--   1 l vegetable oil + 0.2 l methanol = 0.9 l biodiesel
--
oilToBlend :: Blend -> GCM BlendRec
oilToBlend blend = do
  -- Prices for methanol and petrol:
  --
  --   Methanol:            1.5 EUR/l
  --   Petrol diesel:       1.0 EUR/l
  let methanolPrice = 1.5
      petrolPrice   = 1.0

  -- Declare ports
  dieselPort    <- createPort
  petrolPort    <- createPort
  methanolPort  <- createPort
  oilPort       <- createPort
  profitPort    <- createPort

  -- TODO This is local. Is there a better way to declare it?
  --      Alternatively, we could expose it to see how much pure
  --      biodiesel we manufactured.
  biodieselPort <- createPort


  -- Constrain the values at the ports.
  component $ do
    petrol    <- value petrolPort
    methanol  <- value methanolPort
    oil       <- value oilPort
    biodiesel <- value biodieselPort
    diesel    <- value dieselPort
    profit    <- value profitPort

    -- Get tabulated values for blend.
    let (frac, eur, tax) = blendTable blend

    -- Compute amount of pure biodiesel,
    -- 'finished' product (i.e. biodiesel blend; here 'diesel'),
    -- and profit (after taxes are deducted).
    --
    assert $ 0.9 * biodiesel === oil + 0.2 * methanol
    assert $ diesel === lit frac * biodiesel + (1 - lit frac) * petrol
    assert $ profit === diesel * lit (eur * (1 - tax))
                      - methanol * lit methanolPrice
                      - petrol   * lit petrolPrice

  -- Automatically build BlendRec record from variables in scope.
  return BlendRec {..}

data DieselProduction =
  DieselProduction
    { b5Port     :: Port Diesel
    , b30Port    :: Port Diesel
    , b100Port   :: Port Diesel
    , euroPort   :: Port Euro
    , vegOilPort :: Port Oil
    }

--   Petrol availability: 150,000 l
dieselProduction :: GCM DieselProduction
dieselProduction = do
  let petrolAvail = 150000

  b5Port     <- createPort
  b30Port    <- createPort
  b100Port   <- createPort
  euroPort   <- createPort
  vegOilPort <- createPort

  -- Get data for blends.
  blendPorts <- mapM oilToBlend [B5, B30, B100]

  component $ do
    profits <- mapM (value . profitPort) blendPorts
    diesels <- mapM (value . dieselPort) blendPorts
    petrols <- mapM (value . petrolPort) blendPorts
    oils    <- mapM (value . oilPort)    blendPorts

    euro   <- value euroPort
    b5     <- value b5Port
    b30    <- value b30Port
    b100   <- value b100Port
    vegOil <- value vegOilPort

    -- Set euro port to sum of profits. Also, assert that we only use
    -- available petrol. Finally, assert that vegetable oil used is in
    -- range of what is available.
    assert $ euro === sum profits
    assert $ sum oils `inRange` (0, vegOil)
    assert $ sum petrols `inRange` (0, lit petrolAvail)

    -- Assert that blends correspond
    mapM_ (\(x, y) -> assert $ x === y) $ zip diesels [b5, b30, b100]

  return DieselProduction {..}


-- * Optimization
-------------------------------------------------------------------------------

{--- TODO: Unable to use this on account of createGoal being hardcoded to-}
{--- integers.-}
{-minimize :: Port Float -> GCM ()-}
{-minimize p = do-}
  {-g <- createGoalFloat-}
  {-linkBy (fun negate) p g-}

-- | Maximize profits.
--
-- The consumer demand of biodiesel at 280,000 l must be met.
--
-- TODO: Add goal for Floats.
problem :: GCM ()
problem = do
  let demand = 280000

  -- Create system
  oilProd               <- oilProduction
  DieselProduction {..} <- dieselProduction
  link oilProd vegOilPort

  -- Assert demand is exceeded
  component $ do
    oils <- sequence [ value p | p <- [b5Port, b30Port, b100Port]]
    assert $ lit demand .<= sum oils

  output b5Port   "B5"
  output b30Port  "B30"
  output b100Port "B100"
  output oilProd  "Vegetable oil"
  output euroPort "Profits EUR"

main :: IO ()
main = putStr =<< runGCM False problem
