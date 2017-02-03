{-# LANGUAGE DeriveGeneric #-}
import System.Environment
import Data.Aeson
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as FingBS

{-
 - TODO
 -  * Hand-craft a nice JSON format
 -  * Run the generated program
 -}

data Graph = Graph { nodes :: [FilledOutNode] }
  deriving (Generic, Show, Eq)
instance ToJSON Graph where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Graph

data FilledOutNode = FilledOutNode { nodeIdentity     :: Int
                                   , componentName    :: String
                                   , filledParameters :: [FilledOutParameter]
                                   , filledInterface  :: [FilledOutInterface]
                                   }
  deriving (Generic, Show, Eq)
instance ToJSON FilledOutNode where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FilledOutNode

data FilledOutParameter = FilledOutParameter { parameterName  :: String
                                             , parameterType  :: PrimType
                                             , parameterValue :: PrimTypeValue
                                             }
  deriving (Generic, Show, Eq)
instance ToJSON FilledOutParameter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FilledOutParameter

data FilledOutInterface = FilledOutInterface { portName :: String
                                             , portType :: String
                                             , portConn :: Maybe (Int, String)
                                             }
  deriving (Generic, Show, Eq)
instance ToJSON FilledOutInterface where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FilledOutInterface

data PrimType = FloatT | IntT | StringT | BoolT
  deriving (Generic, Show, Eq)
instance ToJSON PrimType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PrimType

-- We need to fix the instance here...
data PrimTypeValue = FloatV Float | IntV Int | StringV String | BoolV Bool
  deriving (Generic, Show, Eq)
instance ToJSON PrimTypeValue where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PrimTypeValue

example = Graph
  [ FilledOutNode
     1
     "pump"
     [ FilledOutParameter "capacity" FloatT (FloatV 5) ]
     [ FilledOutInterface "inflow" "flow" (Just (3, "outlet"))
     , FilledOutInterface "outflow" "flow" Nothing
     ]
  , FilledOutNode
     2
     "rain"
     [ FilledOutParameter "amount" FloatT (FloatV 10) ]
     [ FilledOutInterface "rainfall" "flow" (Just (3, "inflow")) ]
  , FilledOutNode
     3
     "runoffArea"
     [ FilledOutParameter "capacity" FloatT (FloatV 10) ]
     [ FilledOutInterface "inflow"   "flow" Nothing
     , FilledOutInterface "outlet"   "flow" Nothing
     , FilledOutInterface "overflow" "flow" Nothing
     ]
  ]
      

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
portLinks n = [ "link id" ++ show (nodeIdentity n) ++ portName p ++ " id" ++ (show i) ++ s
              | p <- filledInterface n
              , isJust (portConn p)
              , let (i, s) = fromJust (portConn p)]

bindVariables :: Graph -> [String]
bindVariables g = ["(" ++ intercalate "," (portNames n) ++ ")" | n <- nodes g]

nodeInitialisations :: Graph -> [String]
nodeInitialisations g = zipWith (\l r -> l ++ "<-" ++ r) (bindVariables g) (components g)

outputStatements :: Graph -> [String]
outputStatements g = ["output " ++ pn ++ " \"" ++ pn ++ "\"" | pn <- allPortNames g]

generateFile :: Graph -> String
generateFile g =  unlines (imports g)
               ++ "\ngraph = do\n"
               ++ unlines ["  " ++ l | l <- nodeInitialisations g ++ allPortLinks g ++ outputStatements g] 
               ++ "\nmain = runGCM graph"

main = do
  args <- getArgs
  let g = head args
  Just gr <- (decode . FingBS.pack) <$> readFile g
  writeFile "model.hs" (generateFile gr)
