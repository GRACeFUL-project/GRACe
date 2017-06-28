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
--   Crop            Yield [t/ha]   Water demand [Ml/ha]  Oil content [l/t]
--   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   Soybeans         3              5                     178
--   Sunflower seeds  2              4                     216
--   Cotton seeds     1              1                     433
--
-- Note the units.
cropTable :: Crop -> (Yield, Water, Oil)
cropTable Soy       = (3, 5, 178)
cropTable Sunflower = (2, 4, 216)
cropTable Cotton    = (1, 1, 433)

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

  -- Area (in ha) used to grow crop.
  areaPort  <- createPort
  -- Amount of oil produced from crop.
  oilPort   <- createPort
  -- Amount of water used by crop.
  waterPort <- createPort

  -- Constrain the values at the ports.
  component $ do
    areaValue  <- value areaPort
    oilValue   <- value oilPort
    waterValue <- value waterPort

    -- Get tabulated values for the crop.
    let (y, w, o) = cropTable crop

    -- Calculate values from data.
    assert $ oilValue   === lit y * lit o * areaValue
    assert $ waterValue === lit w * areaValue

  return (areaPort, oilPort, waterPort)

-- | GCM component for optimizing crop production to provide the maximum amount
--   of oil for the available land and water.
oilProduction :: Area -> Water -> GCM (Port Oil, Port Water, Port Area, Port Area, Port Area)
oilProduction farmland water = do

  -- Port for total vegetable oil yield.
  oilYieldPort <- createPort
  -- Port for amount of water used.
  waterPort <- createPort
  -- Ports for area used to grow each crop.
  soyPort  <- createPort
  sunfPort <- createPort
  cottPort <- createPort

  -- Get the relevant ports for all crops.
  cropPorts <- mapM cropToOil [Soy, Sunflower, Cotton]

  component $ do
    -- Values for crops: (area, water, oil)
    areas@[soyArea, sunfArea, cottArea]  <-
      sequence [ value ap | (ap, _, _) <- cropPorts ]
    waters <- sequence [ value wp | (_, _, wp) <- cropPorts ]
    oils   <- sequence [ value op | (_, op, _) <- cropPorts ]

    yield  <- value oilYieldPort
    watertotal <- value waterPort
    soy <- value soyPort
    sunf <- value sunfPort
    cott <- value cottPort

    --  The total area of crops is non-negative and is bounded by the available
    --  farmland. Each crop area is also non-negative.
    assert $ sum areas `inRange` (0, lit farmland)
    mapM_ (\x -> assert $ 0 .<= x) areas
    -- The total amount of water used is non-negative and is bounded by the
    -- available water reservoir. The amount for each crop is also non-negative.
    assert $ sum waters `inRange` (0, lit water)
    mapM_ (\x -> assert $ 0 .<= x) waters

    -- We constrain the values we are interested in to our ports.
    assert $ yield === sum oils
    assert $ watertotal === sum waters
    assert $ soy === soyArea
    assert $ sunf === sunfArea
    assert $ cott === cottArea

  return (oilYieldPort, waterPort, soyPort, sunfPort, cottPort)

-- | Help function for maximizing goal.
maximize :: Port Int -> GCM ()
maximize p = do
  g <- createGoal
  link p g

-- | In this example the following quantities are available:
--
--   Farmland: 1,600 ha
--   Water:    5,000 Ml
--
problem :: GCM ()
problem = do
  -- Create system
  (oil, water, soy, sun, cot) <- oilProduction 1600 5000

  -- Our goal is to maximize the amount of oil produced.
  maximize oil

  -- We print the values we would like to see.
  output oil "Oil produced"
  output water "Water used"
  output soy "Soybean area"
  output sun "Sunflower area"
  output cot "Cotton area"

main :: IO ()
main = print =<< runGCM problem
