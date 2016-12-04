import SmallExample as Small
import Examples as E
import GL

main = do
  runGCM Small.example
  runGCM E.example
  runGCM E.energySystem
