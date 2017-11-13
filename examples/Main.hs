module Main where

import SmallExample as Small
import Examples as E
import GCM
import Compile

main = do
  runCompare False Small.example
  runCompare False E.example
  runCompare False E.energySystem
