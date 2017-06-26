{-# LANGUAGE TypeApplications #-}
import Control.Monad
import qualified Data.HashMap.Lazy as HM
import Data.List.Split

import System.Process
import TestFW.GCMCheck
import TestFW.GCMP
import qualified Test.QuickCheck as QC
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import qualified Data.Text as T
import Data.List

import Compile0
import GCM
import CP

  -- TODO: update to fix the Variable / Port type mismatch

data Pump = Pump {inflow :: Port Int, outflow :: Port Int, capacity :: Param Int}

-- A pump with a initial capacity c
pump :: Int -> GCM Pump
pump c = do
  iflow <- createPort
  oflow <- createPort
  cap  <- createParam c
  component $ do
    ifl <- value iflow
    ofl <- value oflow
    c <- value cap
    assert $ ifl `inRange` (0, c)
    assert $ ofl === ifl
  return (Pump iflow oflow cap)

prop_pump :: GCMP ()
prop_pump = do
  k <- forall (fmap abs QC.arbitrary)

  pmp <- liftGCM $ pump k

  vi <- value (inflow pmp)
  property $ vi .< lit k

-- | Runs a prog expected to contain "prop_..." output
-- and tests if that output contained any "prop_... = false"
run :: GCM a -> IO Bool
run prog = do
  writeFile "model.mzn" (compileGCM prog)
  callCommand "mzn2fzn model.mzn"
  out <- readCreateProcess (shell "fzn-gecode -p 4 -n 10 model.fzn | solns2out --soln-sep \"\" --search-complete-msg \"\" model.ozn") ""
  callCommand "rm model.mzn model.ozn model.fzn"
  return (wasSucess out)

-- | Tests that all label starting with "prop_" in MiniZinc output are True
wasSucess :: String -> Bool
wasSucess s =
  let keyValue = [HM.toList hm
                 | Object hm <-
                    map fromJust $
                    filter isJust $
                    map ((decode @Value) . BS.pack) $
                    filter (not . null) $
                    splitOn "\n\n" s
                ]
      bools = [b | (label, Bool b) <- (concat keyValue), "prop_" `isPrefixOf` (T.unpack label)]
  in  and bools

test :: GCMP a -> IO ()
test prop = do
  let generator = makeGenerator prop
  results <- replicateM 10 $ QC.generate generator >>= run
  if and results then
    putStrLn "Success"
  else
    putStrLn "Failure"

main = test prop_pump
