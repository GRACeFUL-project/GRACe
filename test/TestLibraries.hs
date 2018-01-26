module Main where
import TestFW.GCMCheck
import qualified CrudProperties

main :: IO ()
main = do
  check "Pump" CrudProperties.prop_pump
  check "Rain" CrudProperties.prop_rain
  check "Storage" CrudProperties.prop_runoff -- This one takes a while
