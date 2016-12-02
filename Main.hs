import SmallExample as Small
import Examples as E
import GCM

main = do
  runGCM Small.example
  runGCM E.example
  runGCM E.energySystem
