module Main where
import Compile(run)
import GCM      ( GCM, output, component, createIntGoal
                , Port, createPort, link, value
                )
import CP       ( assert, lit, (===), (.<=), inRange )


-- * Vegetable oil manufacturing
-------------------------------------------------------------------------------
-- We define a system to describe growing and producing vegetable oil from
-- different types of crops.

-- | We use type synonyms to keep track of the different resources
--   we are working with.
--
-- Farmland area is measured in ha
type Area  = Int
-- Crop yield is measure in t/ha
type Yield = Int
-- Water is measured in Ml
type Water = Int
-- Oil is measured in l
type Oil   = Int

-- Each crop has parameters describing its yield in t/ha,
-- its water demand in Ml/ha, and its oil content in l/t.
type CropParams = (Yield, Water, Oil)

-- | GCM component for a single crop.
--
-- The component is parametrized on the crop's parameters and computes the
-- oil yield (in l) and water consumption (in Ml), given that we grow
-- so-and-so many ha of this crop.
crop :: CropParams -> GCM (Port Area, Port Water, Port Oil)
crop (y,w,o) = do
  -- Area (in ha) used to grow crop.
  areaPort  <- createPort

  -- Amount of water used by crop.
  waterPort <- createPort
  -- Amount of oil produced from crop.
  oilPort   <- createPort

  -- Constrain the values at the ports.
  component $ do
    areaValue  <- value areaPort
    oilValue   <- value oilPort
    waterValue <- value waterPort

    -- Calculate values from data.
    assert $ oilValue   === lit y * lit o * areaValue
    assert $ waterValue === lit w * areaValue

  return (areaPort, waterPort, oilPort)

-- | GCM component for farmland.
--
-- The component is parametrized on the available amount of land (in ha)
-- and the number of different crops available to grow, and has ports
-- describing how the land is divided between the crops.
farm :: Area -> Int -> GCM [Port Area]
farm land numCrops = do
  -- Create a port for each crop.
  areaPorts <- mapM (\_ -> createPort) (take numCrops (repeat 0))
  component $ do
    areaVals <- sequence [value ap | ap <- areaPorts]
    --  The total area of crops is non-negative and is bounded by the available
    --  farmland. Each crop area is also non-negative.
    assert $ sum areaVals `inRange` (0, lit land)
    mapM_ (\x -> assert $ 0 .<= x) areaVals
  return areaPorts

-- | GCM component for water usage.
--
-- The component is parametrized on the available amount of water (in Ml)
-- and the number of different crops available to grow, and has ports
-- describing how the water is divided between the crops.
reservoir :: Water -> Int -> GCM [Port Water]
reservoir waterSource numCrops = do
  -- Create a port for each crop.
  waterPorts <- mapM (\_ -> createPort) (take numCrops (repeat 0))
  component $ do
    waterVals <- sequence [value wp | wp <- waterPorts]
    -- The total amount of water used is non-negative and is bounded by the
    -- available water reservoir. The amount for each crop is also non-negative.
    assert $ sum waterVals `inRange` (0, lit waterSource)
    mapM_ (\x -> assert $ 0 .<= x) waterVals
  return waterPorts

-- | GCM component for oil production.
--
-- The component is parametrized on the number of different crops available
-- to grow, and has a list of ports describing how much oil is produced by
-- each crop as well as a port containing the total amount of oil produced.
oilProduction :: Int -> GCM ([Port Oil], Port Oil)
oilProduction numCrops = do
  -- Create a port for each crop.
  oilCrops <- mapM (\_ -> createPort) (take numCrops (repeat 0))
  oilOut <- createPort
  component $ do
    oilProduced <- value oilOut
    oilSources <- mapM value oilCrops
    -- The total amount of oil is the sum of the amounts from each crop.
    assert $ oilProduced === sum oilSources
  return (oilCrops, oilOut)

-- | In our example problem we have 3 crops: Soybeans, sunflower seeds and
--   cotton seeds, parametrized by the following table:
--
--   Crop            Yield [t/ha]   Water demand [Ml/ha]  Oil content [l/t]
--   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--   Soybeans         3              5                     178
--   Sunflower seeds  2              4                     216
--   Cotton seeds     1              1                     433
--
-- Note the units.

-- | We define a type for our crops and a function to keep track of
--   parameters
data Crop = Soy | Sunflower | Cotton
cropTable :: Crop -> (Yield, Water, Oil)
cropTable Soy       = (3, 5, 178)
cropTable Sunflower = (2, 4, 216)
cropTable Cotton    = (1, 1, 433)

-- | Help function for maximizing goal.
maximize :: Port Int -> GCM ()
maximize p = do
  g <- createIntGoal
  link p g

-- | GCM program for optimizing the area of land on which to grow each of the 3
--   available crops in order to maximize oil production.
--
--   In this example the following quantities are available:
--   Farmland: 1,600 ha
--   Water:    5,000 Ml
--
problem :: GCM ()
problem = do
  -- Create system
  (soyArea, soyWater, soyOil) <- crop $ cropTable Soy
  (sunArea, sunWater, sunOil) <- crop $ cropTable Sunflower
  (cotArea, cotWater, cotOil) <- crop $ cropTable Cotton

  [soy_a, sun_a, cot_a] <- farm 1600 3
  [soy_w, sun_w, cot_w] <- reservoir 5000 3
  ([soy_o, sun_o, cot_o], oilProduced) <- oilProduction 3

  -- Link the appropriate ports together.
  link soyArea soy_a
  link soyWater soy_w
  link soyOil soy_o

  link sunArea sun_a
  link sunWater sun_w
  link sunOil sun_o

  link cotArea cot_a
  link cotWater cot_w
  link cotOil cot_o

  -- Our goal is to maximize the amount of oil produced.
  maximize oilProduced

  -- We print the values we would like to see.
  output oilProduced "Oil produced"
  output soyArea "Soybean area"
  output sunArea "Sunflower area"
  output cotArea "Cotton area"

main :: IO ()
main = putStr =<< run True problem
