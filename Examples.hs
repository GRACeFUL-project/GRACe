module Examples where
import CP
import Port

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
        outflow <- value op
        assert  $  outflow === min' 0 (inflow - lit capacity)

-- A forked pipe
forkedPipe :: Port Float -> Port Float -> Port Float -> CP ()
forkedPipe ip op1 op2 =
    do
        inflow   <- value ip
        outflow1 <- value op1
        outflow2 <- value op2
        assert   $  inflow === outflow1 + outflow2
