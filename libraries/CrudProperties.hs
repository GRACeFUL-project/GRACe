module CrudProperties( prop_pump
                     , prop_rain)
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
    portVal oflow .> portVal iflow

prop_rain :: GCMP ()
prop_rain = do
  k   <- forall (fmap abs QC.arbitrary)
  r <- liftGCM $ rain k
  vr <- value r
  property "Rainfall equal to parameter" $ vr === lit k
