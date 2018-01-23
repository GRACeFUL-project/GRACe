import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OutParser
import TestFW.GCMCheck

import qualified CrudProperties

main :: IO ()
main = do
  check "Pump" CrudProperties.prop_pump
  check "Rain" CrudProperties.prop_rain
