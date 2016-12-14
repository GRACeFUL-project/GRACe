module TestFW.GCMCheck where

import Control.Monad
import Data.List (isPrefixOf)
import qualified Test.QuickCheck as QC

import Compile0
import TestFW.GCMP
import OutParser

-- | GCM property run function
--
-- Runs the property 100 times and prints the first fail if any.
check :: GCMP a -> IO ()
check prop = do
  results <- replicateM 100 (checkOnce prop)
  case msum results of
    Just sol -> putStrLn (show sol) >> putStrLn "----- Test failed! -----"
    Nothing -> putStrLn "+++++ Test passed! +++++"

-- | Run a GCM property once and return a failing solution if any are found.
checkOnce :: GCMP a -> IO (Maybe Solution)
checkOnce prop = do
  prg <- QC.generate $ makeGenerator prop
  out <- runDef $ compileGCM prg
  return $ case out of
    Sat sols -> msum $ map verify sols
    _ -> Nothing

-- | Check if a solution passes all properties in it.
--
-- verify looks for all labels with the prefix @"prop_"@ and verifies they have
-- the value @True@ assigned to them.
--
-- If the solution fails it is returned otherwise nothing is returned.
verify :: Solution -> Maybe Solution
verify (Sol vars opt) = if propsSat then Nothing else Just $ Sol vars opt
  where
    props = filter (isPrefixOf "prop_" . fst) vars
    propsSat = all ((B True ==) . snd) props
