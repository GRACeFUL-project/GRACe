module SmallExample where

import Compile0
import GL
import GCMP
import qualified Test.QuickCheck as QC

-- A source of rain
rain :: Int -> GCM (Port Int)
rain s =
    do
      p <- createPort
      set p s
      return p

-- A pump with a fixed capacity
pump :: Int -> GCM (Port Int, Param Int)
pump c =
    do
        flow <- createPort
        cap  <- createParam c
        component $ do
                      f <- value flow
                      c <- value cap
                      assert  $ f `inRange` (0, c)
        return (flow, cap)

-- A storage with a fixed capacity and a pump
storage :: Int -> GCM (Port Int, Port Int, Port Int)
storage c =
    do
        inflow   <- createPort
        pump     <- createPort
        overflow <- createPort
        component $ do
                      inf <- value inflow
                      pmp <- value pump
                      ovf <- value overflow
                      assert $ ovf === max' 0 (inf - pmp - lit c)
                      let sumFlow = ovf + pmp
                      assert $ inf `inRange` (sumFlow, sumFlow + lit c)
        return (inflow, pump, overflow)

-- | Increases pump capacity by doubling.
increaseCap :: Param Int -> GCM (Action Int)
increaseCap p = do
    a <- createAction p
    action $ act (\a defaultValue -> 2*a*defaultValue) a
    return a

-- Small example
example :: GCM ()
example =
    do
      -- Instantiate components
      r <- rain 10
      (p, pcap) <- pump 2
      (inf, pmp, ovf) <- storage 4

      -- Create an action
      a  <- increaseCap pcap
      a' <- taken a

      -- Link ports
      link inf r
      link p pmp

      -- Minimise overflow
      g <- createGoal
      fun (\overflow -> negate overflow) ovf g

      -- Output the solution
      output p "pump operation"
      output a' "pump increased"

prop_pump :: GCMP ()
prop_pump =
    do
        k <- forall (fmap abs QC.arbitrary)
        
        (pmp, cap) <- liftGCM $ pump k

        {-
        liftGCM $ do
                    p <- createPort
                    set p k
                    output p "k"
                    output pmp "pmp"
        -}

        property $ val pmp .<= lit k 
