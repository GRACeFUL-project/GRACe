module TestFW.GCMCheck where

import Control.Monad
import qualified Test.QuickCheck as QC

import Compile0
import TestFW.GCMP
import OutParser

-- | GCM property run function
check :: GCMP a -> IO ()
check prop = do
  results <- replicateM_ 100 (checkOne prop)


checkOne :: GCMP a -> IO (Maybe Solution)
checkOne prop = do
  prg <- QC.generate $ makeGenerator prop
  out <- runDef $ compileGCM prg
  putStrLn $ show out
