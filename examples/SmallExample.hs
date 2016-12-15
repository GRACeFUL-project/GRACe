module SmallExample where

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
pump c =
    do
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
storage c =
    do
        inflow   <- createPort
        outlet   <- createPort
        overflow <- createPort
        storageC <- createParam c
        currentV <- createPort
        output currentV "current value"
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

-- | Increases pump capacity by doubling.
increaseCap :: Param Int -> GCM (Action Int)
increaseCap p = createAction (+) p

-- Small example
example :: GCM ()
example =
    do
      -- Instantiate components
      r <- rain 10
      pmp <- pump 2
      (inf, out, ovf, cap) <- storage 4

      -- Create an action
      pumpAction  <- increaseCap (capacity pmp)
      pumpAction' <- taken pumpAction

      storageAction  <- increaseCap cap 
      storageAction' <- taken storageAction

      -- Link ports
      link inf r
      link (inflow pmp) out 

      -- Minimise overflow
      g <- createGoal
      fun (\overflow -> 100 * (negate overflow)) ovf g

      -- Output the solution
      output (inflow pmp)   "pump operation"
      output pumpAction'    "pump increased"
      output storageAction' "storage increased"
      output ovf            "overflow"
      output inf            "inflow"

prop_pump :: GCMP ()
prop_pump =
    do
        k <- forall (fmap abs QC.arbitrary)

        pmp <- liftGCM $ pump k

        {-
        liftGCM $ do
                    p <- createPort
                    set p k
                    output p "k"
                    output pmp "pmp"
        -}

        property $ val (inflow pmp) .<= lit k
