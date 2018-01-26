module CrudProperties( prop_pump
                     , prop_rain
                     , prop_runoff
                     )
  where

import TestFW.GCMP
import qualified Test.QuickCheck as QC
import Crud
import GCM
import CP

prop_pump :: GCMP ()
prop_pump = do
  k   <- forall (fmap abs QC.arbitrary)
  (iflow, oflow) <- liftGCM $ simplePump k
  property "Inflow within capacity" $
    portVal iflow .<= lit k
  property "Outflow equal to inflow" $
    portVal oflow === portVal iflow

prop_rain :: GCMP ()
prop_rain = do
  k   <- forall (fmap abs QC.arbitrary)
  r <- liftGCM $ rain k
  property "Rainfall equal to parameter" $ portVal r === lit k

prop_runoff :: GCMP ()
prop_runoff = do
  k   <- forall (fmap abs QC.arbitrary)
  (inf, outf, ovf) <- liftGCM $ simpleRunoffArea k
  property "Outflow <= inflow" $
    portVal outf .<= portVal inf
  property "If overflow, then inflow > outflow" $
    (portVal ovf .> 0) ==> (portVal inf .> portVal outf)
