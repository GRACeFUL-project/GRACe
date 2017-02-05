{-# LANGUAGE DeriveGeneric
  , OverloadedStrings
  , TypeApplications
#-}
module Main where
import System.Environment
import System.Process
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as FingBS
import qualified Data.Text as T

import GRACeGraph

{-
 - TODO
 -  * Run the generated program.
 -}

imports :: Graph -> [String]
imports g = nub $ ["import " ++ capf (name n) | n <- nodes g]
  where
    capf [] = []
    capf (x:xs) = (toUpper x):xs

components :: Graph -> [String]
components g = [name n ++ " " ++ intercalate " " (prettyArgs n) | n <- nodes g]

prettyArgs :: Node -> [String]
prettyArgs n = [pPrintPTV (parameterValue p) | p <- parameters n]

allPortNames :: Graph -> [String]
allPortNames g = concatMap interfaceNames (nodes g)

interfaceNames :: Node -> [String]
interfaceNames n = [ "id" ++ show (fromMaybe 0 (identity n)) ++ interfaceName p | p <- interface n]

allPortLinks :: Graph -> [String]
allPortLinks g = concatMap portLinks (nodes g)

portLinks :: Node -> [String]
portLinks n = [ "link id" ++ (show (fromMaybe 0 (identity n))) ++ interfaceName p ++ " id" ++ (show i) ++ s
              | p <- interface n
              , isJust (interfaceConnection p)
              , let (i, s) = fromJust (interfaceConnection p)]

bindVariables :: Graph -> [String]
bindVariables g = ["(" ++ intercalate "," (interfaceNames n) ++ ")" | n <- nodes g]

nodeInitialisations :: Graph -> [String]
nodeInitialisations g = zipWith (\l r -> l ++ "<-" ++ r) (bindVariables g) (components g)

outputStatements :: Graph -> [String]
outputStatements g = ["output " ++ pn ++ " \"" ++ pn ++ "\"" | pn <- allPortNames g]

-- | Generate a haskell file in `GRACe` from
-- a `Graph`
generateFile :: Graph -> String
generateFile g =  unlines (["import GL", "import Compile0"] ++ imports g)
               ++ "\ngraph = do\n"
               ++ unlines ["  " ++ l | l <- nodeInitialisations g ++ allPortLinks g ++ outputStatements g] 
               ++ "\nmain = runGCM graph"

main = do
  args <- getArgs
  let lib = args !! 0
      g   = args !! 1
  Just gr <- (decode . FingBS.pack) <$> readFile g
  writeFile "model.hs" (generateFile gr)
  callCommand $ "cabal exec -- runhaskell -i" ++ lib ++ " model.hs"
  callCommand "rm model.hs"
