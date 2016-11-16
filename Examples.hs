module Examples where
import CP
import Port
import GCM

-- A pipe with a specific flow capacity
pipe :: Float -> Port Float -> Port Float -> CP ()
pipe f ip op = do
                inflow  <- value ip
                outflow <- value op
                assert  $  inflow === lit f
                assert  $  outflow === inflow

-- A storage facility with a capacity, an inflow, and a runoff
storage :: Float -> Port Float -> Port Float -> CP () 
storage capacity ip op =
    do
        inflow  <- value ip
        overflow <- value op
        assert $ overflow === min' 0 (inflow - lit capacity)

-- A pump with capacity c m^3
pump :: Float -> Port Float -> CP ()
pump c p =
    do
        inflow <- value p
        assert $  inflow .<= lit c

-- X m^3 of rain
rain :: Float -> Port Float -> CP ()
rain x p =
    do
        outflow <- value p
        assert  $  outflow === lit x

-- A pump as a GCM component
pumpGCM :: Float ->  GCM (Port Float)
pumpGCM k =
    do
        p <- createPort
        component $ pump k p
        return p

-- Rain as a GCM component
rainGCM :: Float -> GCM (Port Float)
rainGCM k =
    do
        p <- createPort
        component $ rain k p
        return p
