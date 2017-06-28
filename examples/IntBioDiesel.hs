{-# LANGUAGE RecordWildCards       #-}
-- The first part of the biodiesel example, but with integers instead of floats.
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

type Area  = Int
type Yield = Int
type Water = Int
type Oil   = Int

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
cropTable Soy       = (3, 5, 1)
cropTable Sunflower = (2, 4, 2)
cropTable Cotton    = (1, 1, 4)

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
oilProduction :: GCM (Port Oil, Port Water, Port Area, Port Area, Port Area)
oilProduction = do
  let farmland = 1600
      water    = 5000

  -- Port for total vegetable oil yield.
  oilYieldPort <- createPort

  waterPort <- createPort

  soyPort  <- createPort
  sunfPort <- createPort
  cottPort <- createPort

  -- Get the relevant ports for all crops.
  cropPorts <- mapM cropToOil [Soy, Sunflower, Cotton]

  -- 1. The total area of crops is non-negative and is bounded by the available
  --    farmland.
  -- 2. The total amount of water used is non-negative and is bounded by the
  --    available water reservoir.
  component $ do
    -- Values for crops: (area, water, oil)
    areas  <- sequence [ value ap | (ap, _, _) <- cropPorts ]
    waters <- sequence [ value wp | (_, _, wp) <- cropPorts ]
    oils   <- sequence [ value op | (_, op, _) <- cropPorts ]
    yield  <- value oilYieldPort
    watertotal <- value waterPort
    soy <- value soyPort
    sunf <- value sunfPort
    cott <- value cottPort

    assert $ sum areas `inRange` (0, lit farmland)
    assert $ sum waters `inRange` (0, lit water)
    assert $ sum oils === yield
    assert $ sum waters === watertotal
    assert $ head areas === soy
    assert $ 0 .<= soy
    assert $ (head $ tail areas) === sunf
    assert $ 0 .<= sunf
    assert $ last areas === cott
    assert $ 0 .<= cott

  return (oilYieldPort, waterPort, soyPort, sunfPort, cottPort)

maximize :: Port Int -> GCM ()
maximize p = do
  g <- createGoal
  link p g

problem :: GCM ()
problem = do
  -- Create system
  (oilProd, watertotal, soy, sun, cot) <- oilProduction

  maximize oilProd
  output oilProd "Oil produced"
  output watertotal "Water used"
  output soy "Soybean area"
  output sun "Sunflower area"
  output cot "Cotton area"

main :: IO ()
main = print =<< runGCM problem
