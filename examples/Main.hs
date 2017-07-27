module Main where

import SmallExample as Small
import Examples as E
import GCM
import Compile

main = do
  runCompare Small.example
  runCompare E.example
  runCompare E.energySystem
