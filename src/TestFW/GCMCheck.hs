module TestFW.GCMCheck where

import qualified Test.QuickCheck as QC

import Compile0
import TestFW.GCMP
import OutParser

-- | GCM property run function
check :: GCMP a -> IO ()
check prop = do
  prg <- QC.generate $ makeGenerator prop
  out <- runDef $ compileGCM prg
  putStrLn $ show out
