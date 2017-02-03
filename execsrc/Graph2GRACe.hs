{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char

data Graph = Graph { nodes :: [FilledOutNode] }
  deriving (Generic, Show, Eq)

data FilledOutNode = FilledOutNode { nodeIdentity     :: Int
                                   , componentName    :: String
                                   , filledParameters :: [FilledOutParameter]
                                   , filledInterface  :: [FilledOutInterface]
                                   }
  deriving (Generic, Show, Eq)

data FilledOutParameter = FilledOutParameter { parameterName  :: String
                                             , parameterType  :: PrimType
                                             , parameterValue :: PrimTypeValue
                                             }
  deriving (Generic, Show, Eq)

data FilledOutInterface = FilledOutInterface { portName :: String
                                             , portType :: String
                                             , portConn :: Maybe (Int, String)
                                             }
  deriving (Generic, Show, Eq)

data PrimType = FloatT | IntT | StringT | BoolT
  deriving (Generic, Show, Eq)

data PrimTypeValue = FloatV Float | IntV Int | StringV String | BoolV Bool
  deriving (Generic, Show, Eq)

pPrintPTV :: PrimTypeValue -> String
pPrintPTV (FloatV f)  = show f
pPrintPTV (IntV i)    = show i
pPrintPTV (StringV s) = show s
pPrintPTV (BoolV b)   = show b

imports :: Graph -> [String]
imports g = nub $ ["import " ++ capf (componentName n) | n <- nodes g]
  where
    capf [] = []
    capf (x:xs) = (toUpper x):xs

components :: Graph -> [String]
components g = [componentName n ++ " " ++ intercalate " " (prettyArgs n) | n <- nodes g]

prettyArgs :: FilledOutNode -> [String]
prettyArgs n = [pPrintPTV (parameterValue p) | p <- filledParameters n]

allPortNames :: Graph -> [String]
allPortNames g = concatMap portNames (nodes g)

portNames :: FilledOutNode -> [String]
portNames n = [ "id" ++ show (nodeIdentity n) ++ portName p | p <- filledInterface n]

allPortLinks :: Graph -> [String]
allPortLinks g = concatMap portLinks (nodes g)

portLinks :: FilledOutNode -> [String]
portLinks n = [ "link id" ++ show (nodeIdentity n) ++ portName p ++ " " ++ (show i) ++ s | p <- filledInterface n, isJust (portConn p), let (i, s) = fromJust (portConn p)]

bindVariables :: Graph -> [String]
bindVariables g = ["(" ++ intercalate "," (portNames n) ++ ")" | n <- nodes g]

nodeInitialisations :: Graph -> [String]
nodeInitialisations g = zipWith (\l r -> l ++ "<-" ++ r) (bindVariables g) (components g)

outputStatements :: Graph -> [String]
outputStatements g = ["output " ++ pn ++ "\"" ++ pn ++ "\"" | pn <- allPortNames g]

generateFile :: Graph -> String
generateFile g =  unlines (imports g)
               ++ "\ngraph = do\n"
               ++ unlines ["  " ++ l | l <- nodeInitialisations g ++ allPortLinks g ++ outputStatements g] 
               ++ "\nmain = runGCM graph"
