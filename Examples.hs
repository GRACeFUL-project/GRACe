{-# LANGUAGE DuplicateRecordFields #-}
module Examples where
import CP
import Port
import GCM

-- A source of flow a
source :: (CPType a, Eq a) => a -> GCM (Port a)
source a =
    do
        p <- createPort
        component $ do
                      outflow <- value p
                      assert $ outflow === lit a
        return p

-- A sink of capacity a
sink :: (CPType a, Ord a, Num a) => a -> GCM (Port a)
sink a =
    do
        p <- createPort
        component $ do
                      inflow <- value p 
                      assert $ inflow `inRange` (0, lit a)
        return p

-- Rain is a source of Floats
rain :: Float -> GCM (Port Float)
rain = source

-- A pipe as a GCM component
data Pipe = Pipe {inflow  :: Port Float,
                  outflow :: Port Float}

-- A pipe with a fixed capacity
pipe :: Float -> GCM Pipe
pipe k =
    do
        ip <- createPort
        op <- createPort
        component $ do
                      inflow  <- value ip
                      outflow <- value op
                      assert  $ inflow `inRange` (0, lit k)
                      assert  $ outflow === inflow
        return $ Pipe ip op

-- A pump with a variable capacity as a GCM component
data Pump = Pump {capacity :: Port Float,
                  inflow   :: Port Float,
                  outflow  :: Port Float}

-- GCM component
pump :: GCM Pump 
pump =
    do
        cp <- createPort
        ip <- createPort
        op <- createPort
        link cp ip
        link ip op
        return $ Pump cp ip op

-- Storage as a GCM component
storage :: Float -> GCM (Port Float, Port Float)
storage k = fun (\inflow -> max' 0 (inflow - lit k))

-- Simple example with outputs and everything
example :: GCM ()
example =
    do
      rainP <- rain 10
      (sin, sof) <- storage 3
      link rainP sin
      output sof "overflow"
      output sin "inflow"
