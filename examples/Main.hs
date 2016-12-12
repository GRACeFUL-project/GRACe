module Main where

import SmallExample as Small
import Examples as E
import GL
import Compile0

main = do
  runGCM Small.example
  runGCM E.example
  runGCM E.energySystem
