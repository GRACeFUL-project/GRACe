{-# LANGUAGE DuplicateRecordFields #-}
module Examples where
import CP
import Port
import GCM

-- A source of flow a
source :: (CPType a) => a -> GCM (Port a)
source a =
    do
        p <- createPort
        set p a
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

-- GCM component to represent energy requirement
type Per = Float
type Eff = Float

requirement :: [(Port Float, Per, Eff)] -> GCM (Port Float)
requirement xs =
    do
        totalP <- createPort
        component $
           do
            total <- value totalP 
            sequence_ [do
                         v <- value p
                         assert $ total*(lit per) === v*(lit eff)
                      | (p, per, eff) <- xs]
        return totalP

-- A GCM representing a simluation of the swedish energy system
energySystem :: GCM ()
energySystem =
    do
       (bio_outs, biobr) <- sumGCM 5
       (foss_outs, foss) <- sumGCM 5
       vind              <- createPort
       nuclear           <- createPort
       hydro             <- createPort
       (waste_heat_outs, waste_heat)        <- sumGCM 2
       heatpump          <- createPort
       (district_heating_outs, district_heating)  <- sumGCM 2
       propulsion        <- createPort
       (electricity_outs, electricity) <- sumGCM 4

       transport         <- requirement [(propulsion, 1, 0.2)]
       set transport 18

       industry_heat     <- requirement [(bio_outs !! 0, 0.4, 0.9), (foss_outs !! 2, 0.56, 0.9), (district_heating_outs !! 0, 0.04, 0.81)]
       set industry_heat 121

       living_heat       <- requirement [(bio_outs !! 1, 0.13, 0.8), (district_heating_outs !! 1, 0.49, 0.81), (foss_outs !! 4, 0.15, 0.8), (electricity_outs !! 1, 0.23, 0.9)]
       set living_heat 90

       living_elec       <- requirement [(electricity_outs !! 2, 1, 0.9)]
       set living_elec 55

       industry_elec    <- requirement [(electricity_outs !! 3, 1, 0.9)]
       set industry_elec 53

       district_heating_rec <- requirement [(bio_outs !! 2, 0.61, 0.85), (foss_outs !! 3, 0.1, 0.85), (heatpump, 0.22, 3), (waste_heat_outs !! 1, 0.07, 1)]
       link district_heating_rec district_heating

       heatpump_rec <- requirement [(electricity_outs !! 0, 1, 0.9)]
       link heatpump heatpump_rec

       propulsion_rec <- requirement [(bio_outs !! 3, 0.06, 0.5), (foss_outs !! 0, 0.94, 0.9)]
       link propulsion propulsion_rec

       electricity_rec <- requirement [(bio_outs !! 4, 0.04, 0.3), (foss_outs !! 1, 0.02, 0.35), (vind, 0.03, 1), (nuclear, 0.39, 0.34), (hydro, 0.48, 1), (waste_heat_outs !! 0, 0.04, 1)]
       link electricity_rec electricity

       output biobr "bio\\t"
       output foss "fossile\\t"
       output vind "wind\\t"
       output nuclear "nuclear\\t"
       output hydro "hydro\\t"
