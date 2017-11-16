{-#LANGUAGE ScopedTypeVariables #-}
import Library
import CLDlib
import Compile0
import Compile1
import Compile
import qualified Interfaces.MZPrinter as HZPrinter
import System.Process
import Control.Monad.State.Lazy

runWithHZ :: Bool -> GCM a -> IO String
runWithHZ showAll gcm = do
  writeFile   "model.mzn" (HZPrinter.layoutModel $ compileGCM' gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", k
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ","
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  callCommand "rm model.mzn model.ozn model.fzn"
  return res
  where k = case showAll of
          True -> param
          False -> "-1"

runComp :: Bool -> GCM a -> IO String
runComp showAll gcm = do
  writeFile   "model.mzn" (compileGCM gcm)
  callCommand "mzn2fzn model.mzn"
  out <- readProcess "fzn-gecode" [ "-p", "4"
                                  , "-n", k
                                  , "model.fzn"] ""
  res <- readProcess "solns2out"  [ "--soln-sep", ""
                                  , "--search-complete-msg", ""
                                  , "model.ozn"] out
  callCommand "rm model.mzn model.ozn model.fzn"
  return res
  where k = case showAll of
          True -> param
          False -> "-1"

-- | Run using both compilers to compare results.
runCompare' :: Bool -> GCM a -> IO ()
runCompare' showAll ex = do
  putStrLn ""
  putStrLn "Results via old compiler:"
  putStr =<< runComp showAll ex
  putStrLn "Results via haskelzinc:"
  putStr =<< runWithHZ showAll ex

simple :: GCM ()
simple = do
  (p1 :: Port Sign) <- createPort
  (p2 :: Port Sign) <- createPort
  (s :: Port Int) <- createPort
  g <- createIntGoal
  link s g
  component $ do
    v1 <- value p1
    v2 <- value p2
    vs <- value s
    assert $ (v1 === 0 .&& v2 /== 0) ==> (vs === 5)
    assert $ (v2 === 0 .&& v1 /== 0) ==> (vs === 5)
    assert $ (v1 /== 0 .&& v2 /== 0) ==> (vs === 0)
    assert $ (v1 === 0 .&& v2 === 0) ==> (vs === 10)
    assert $ v1 === 2 .|| v2 === 2
  output p1 "p1"
  output p2 "p2"
  output s  "s"

stakesExample2 :: GCM ()
stakesExample2 = do
  let bud = 20

  -- Nodes
  bioswale <- actionNode [Z,P] [0,10] 0 2
  fparking <- actionNode [Z,P] [0,15] 0 2

  waterStorage <- cldNode Nothing 2 1
  flooding     <- cldNode Nothing 1 1

  greenSpace   <- funNode 1 0
  nuisance     <- funNode 1 0
  pcap         <- funNode 1 0

  -- value-cost pairs for actions
  --(bioInput,        bioCost)  <- attachFunction [Z,P] [0,10]
  --(parkingInput, parkingCost) <- attachFunction [Z,P] [0,15]

  -- stakeholder 1 wants more green spaces and less nuisance, doesn't care about parking
  --let s1 = ([P,M,Z], [0.67,0.33,0])
  ([g1,n1,p1], h1) <- stakeHolder [[P],[P,M,Z],[P,M,Z]] [1, 0, 0]
  -- stakeholder 2 wants less nuisance and more parking
  --let s2 = ([Z,M,P],[0,0.67,0.33])
  ([g2,n2,p2], h2) <- stakeHolder [[P,M,Z],[P,M,Z],[P]] [0,0,1]

  -- stakeholder 3 wants more parking
  ([g3,n3,p3],h3) <- stakeHolder [[P,M,Z],[P,M,Z],[P,M,Z]] [0,0,0]

  (budgetPorts, totalCost) <- budget 2 bud
  optimisePorts <- optimise 3

  zipWithM link [g1, n1, p1]
                [port greenSpace, port nuisance, port pcap]
  zipWithM link [g2, n2, p2]
                [port greenSpace, port nuisance, port pcap]
  zipWithM link [g3, n3, p3]
                [port greenSpace, port nuisance, port pcap]

  zipWithM link budgetPorts [acost bioswale, acost fparking]

  zipWithM link (fst optimisePorts) [h1,h2,h3]

  cldLink P (acout 0 bioswale)     (inc 0 waterStorage)
  cldLink P (acout 1 bioswale)     (inc 0 greenSpace)
  cldLink P (acout 0 fparking)     (inc 1 waterStorage)
  cldLink P (acout 1 fparking)     (inc 0 pcap)
  cldLink M (out 0 waterStorage) (inc 0 flooding)
  cldLink P (out 0 flooding)     (inc 0 nuisance)

  output (port greenSpace) "greenSpace value"
  --output greenSpaceBenefit "greenspace benefit"
  output (port nuisance) "nuisance value"
  --output nuisanceBenefit "nuisance benefit"
  output (port pcap) "pcap value"
  --output pcapBenefit "pcap benefit"
  output h1 "Happiness of stakeholder 1"
  output h2 "Happiness of stakeholder 2"
  output h3 "Happiness of stakeholder 3"
  output (snd optimisePorts) "Total happiness"
  output totalCost "Total cost"
param :: String
param = "10"

main :: IO ()
main = do
  runCompare' False simple
  runCompare' False stakesExample2
  --compileString simple
  --compileMZ simple
