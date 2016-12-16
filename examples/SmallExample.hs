module SmallExample where

import TestFW.GCMCheck
import Compile0
import GL
import TestFW.GCMP
import qualified Test.QuickCheck as QC

-- A source of rain
rain :: Int -> GCM (Port Int)
rain s =
    do
      p <- createPort
      set p s
      return p

data Pump = Pump {inflow :: Port Int, outflow :: Port Int, capacity :: Param Int}

-- A pump with a fixed capacity
pump :: Int -> GCM Pump
pump c = do
  iflow <- createPort
  oflow <- createPort
  cap  <- createParam c
  component $ do
                ifl <- value iflow
                ofl <- value oflow
                c <- value cap
                assert $ ifl `inRange` (0, c)
                assert $ ofl === ifl
  return (Pump iflow oflow cap)

-- A storage with a fixed capacity and a pump
storage :: Int -> GCM (Port Int, Port Int, Port Int, Param Int)
storage c = do
  inflow   <- createPort
  outlet   <- createPort
  overflow <- createPort
  storageC <- createParam c
  currentV <- createPort
  output currentV "storage contents"
  component $ do
    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    cap <- value storageC
    val <- value currentV
    assert $ inf - out - ovf === val
    assert $ val `inRange` (0, cap)
    assert $ (ovf .> 0) ==> (val === cap)
    assert $ ovf .>= 0
  return (inflow, outlet, overflow, storageC)

type Cost = Int

-- | Increases pump capacity by doubling.
increaseCap :: Param Int -> (CPExp Int -> CPExp Cost) -> GCM (Action Int, Port Cost)
increaseCap p costFunction = do
  a        <- createAction (+) p
  a'       <- taken a
  costPort <- createPort

  fun costFunction a' costPort

  return (a, costPort)

type Flow = Int

floodingOfSquare :: GCM (Port Flow, Port Bool)
floodingOfSquare = do
  flow <- createPort
  isFlooded <- createPort
  
  fun (.> 0) flow isFlooded
  return (flow, isFlooded)

minimize :: Port Int -> GCM ()
minimize p = do
  g <- createGoal
  fun negate p g

maximize :: Port Int -> GCM ()
maximize p = do
  g <- createGoal
  fun id p g

-- Small example
example :: GCM ()
example = do
  let budget = 10000
  -- Instantiate components
  r <- rain 20
  pmp <- pump 2
  (inf, out, ovf, cap) <- storage 4
  (floodFlow, isFlooded) <- floodingOfSquare

  -- Create an action
  (pumpAction, pumpCost) <- increaseCap (capacity pmp) (^2)
  pumpAction' <- taken pumpAction

  (storageAction, storageCost) <- increaseCap cap (*2)
  storageAction' <- taken storageAction

  -- Link ports
  link inf r
  link (inflow pmp) out 
  link ovf floodFlow

  -- We don't want flooding
  set isFlooded False

  totalCost <- createPort
  component $ do
    pc <- value pumpCost
    sc <- value storageCost
    tc <- value totalCost
    assert $ tc === pc + sc
    assert $ tc .<= budget

  minimize totalCost

  -- Output the solution
  output totalCost      "total cost"
  output (inflow pmp)   "pump operation"
  output pumpAction'    "pump increased"
  output storageAction' "storage increased"
  output ovf            "overflow"
  output inf            "inflow"
  output isFlooded      "is flooded"

prop_pump :: GCMP ()
prop_pump = do
  k <- forall (fmap abs QC.arbitrary)

  pmp <- liftGCM $ pump k

  property $ val (inflow pmp) .< lit k
