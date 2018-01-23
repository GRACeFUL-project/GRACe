module TestFW.GCMCheck where

import Control.Monad
import Data.List (isPrefixOf)
import qualified Test.QuickCheck as QC

import Compile0
import TestFW.GCMP
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

type Solution = M.Map String Bool

-- | GCM property run function
--
-- Runs the property 100 times and prints the first fail if any.
check :: String -> GCMP a -> IO ()
check pname prop = do
  putStrLn $ "Checking " ++ pname ++ "..."
  results <- sequence [checkOnce prop | i <- [0..99]]
  putStr "\r"
  case msum results of
    Just sol -> do
      putStrLn $ "Failing property: " ++ (show $ M.toList sol)
      putStrLn "----- Test failed! -----"
    Nothing -> putStrLn "+++++ Test passed! +++++"

-- | Run a GCM property once and return a failing solution if any are found.
checkOnce :: GCMP a -> IO (Maybe Solution)
checkOnce prop = do
  prg <- QC.generate $ makeGenerator prop
  out <- runGCM False prg
  let sols = decode $ BS.pack out :: Maybe Solution
  return $ case sols of
    Just s -> verify s
    Nothing -> Nothing

-- | Check if a solution passes all properties in it.
--
-- verify looks for all labels with the prefix @"prop_"@ and verifies they have
-- the value @True@ assigned to them.
--
-- If the solution fails it is returned otherwise nothing is returned.
verify :: Solution -> Maybe Solution
verify m = if propsSat then Nothing else Just $ m
  where
    props = M.filterWithKey (\k _ -> isPrefixOf "prop_" k) m
    propsSat = and (M.elems props)
