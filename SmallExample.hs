import CP
import Port
import GCM

-- A source of rain
rain :: Int -> GCM (Port Int)
rain s = 
    do
      p <- createPort
      set p s
      return p 

-- A pump with a fixed capacity
pump :: Int -> GCM (Port Int)
pump c =
    do
        flow <- createPort
        component $ do
                      f <- value flow
                      assert  $ f `inRange` (0, lit c)
        return flow

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

-- Small example
example :: GCM ()
example =
    do
      -- Instantiate components
      r <- rain 10
      p <- pump 7
      (inf, pmp, ovf) <- storage 4

      -- Link ports
      link inf r
      link p pmp

      -- Minimise overflow
      g <- createGoal
      fun (\overflow -> negate overflow) ovf g

      -- Output the solution
      output p "pump operation"
