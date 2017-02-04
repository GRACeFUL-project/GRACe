{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import System.Environment
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as FingBS
import qualified Data.Text as T

{-
 - TODO
 -  * Hand-craft a nice JSON format.
 -  * Factor out the graph representation,
 -    we should use it with as many Maybe etc.
 -    as possible to make sure we only have _one_
 -    representation present in both Lib2JSON and
 -    Graph2GRACe.
 -  * Run the generated program.
 -  * `Interface` needs a better json instance
 -}

-- | An adjacency list representation of graphs
data Graph = Graph { nodes :: [Node] }
  deriving (Generic, Show, Eq)

-- | We don't need any fancy instances
instance ToJSON Graph where
  toEncoding = genericToEncoding defaultOptions

-- | The default instance is fine
instance FromJSON Graph

-- | Nodes in the graph
data Node = Node { identity   :: Maybe Int   -- * What ID does this node have?
                 , name       :: String      -- * What is the name of the GL component?
                 , parameters :: [Parameter] -- * What are the parameters of this node?
                 , interface  :: [Interface] -- * What is the interface of this node?
                 }
  deriving (Generic, Show, Eq)

-- | Obvious instances
instance ToJSON Node where
  toEncoding = genericToEncoding $ defaultOptions {omitNothingFields = True}

-- | Obvious instances
instance FromJSON Node where
  parseJSON  = genericParseJSON  $ defaultOptions {omitNothingFields = True}

-- | Fix the prefixes
data Parameter = Parameter { parameterName  :: String
                           , parameterType  :: PrimType
                           , parameterValue :: PrimTypeValue
                           }
  deriving (Generic, Show, Eq)

instance ToJSON Parameter where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Parameter

-- | The primitive types in `GRACe`
data PrimType = FloatT | IntT | StringT | BoolT
  deriving (Generic, Show, Eq)

instance ToJSON PrimType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PrimType

data PrimTypeValue = FloatV Float | IntV Int | StringV String | BoolV Bool
  deriving (Generic, Show, Eq)

-- | PrimTypeValue are represented in JSON as just String/Bool/Number
instance ToJSON PrimTypeValue where
  toJSON (FloatV f)  = Number $ fromRational (toRational f)
  toJSON (IntV i)    = Number $ fromInteger (toInteger i)
  toJSON (StringV s) = String (T.pack s)
  toJSON (BoolV b)   = Bool b

-- | Derived from the above instance
instance FromJSON PrimTypeValue where
  parseJSON (String s) = return $ StringV (T.unpack s)
  parseJSON (Bool b)   = return $ BoolV b
  parseJSON (Number s) = return $ either FloatV IntV $ floatingOrInteger s 
  parseJSON _          = fail "Does not comform to interface"

-- | Fix the prefixes
data Interface = Interface { portName :: String
                           , portType :: String
                           , portConn :: Maybe (Int, String)
                           }
  deriving (Generic, Show, Eq)

instance ToJSON Interface where
  toEncoding = genericToEncoding $ defaultOptions {omitNothingFields = True}

instance FromJSON Interface where
  parseJSON  = genericParseJSON $ defaultOptions {omitNothingFields = True}

example = Graph
  [ Node 
     (Just 1)
     "pump"
     [ Parameter "capacity" FloatT (FloatV 5) ]
     [ Interface "inflow" "flow" (Just (3, "outlet"))
     , Interface "outflow" "flow" Nothing
     ]
  , Node 
     (Just 2)
     "rain"
     [ Parameter "amount" FloatT (FloatV 10) ]
     [ Interface "rainfall" "flow" (Just (3, "inflow")) ]
  , Node 
     (Just 3)
     "runoffArea"
     [ Parameter "capacity" FloatT (FloatV 10) ]
     [ Interface "inflow"   "flow" Nothing
     , Interface "outlet"   "flow" Nothing
     , Interface "overflow" "flow" Nothing
     ]
  ]

pPrintPTV :: PrimTypeValue -> String
pPrintPTV (FloatV f)  = show f
pPrintPTV (IntV i)    = show i
pPrintPTV (StringV s) = show s
pPrintPTV (BoolV b)   = show b

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
allPortNames g = concatMap portNames (nodes g)

portNames :: Node -> [String]
portNames n = [ "id" ++ show (fromMaybe 0 (identity n)) ++ portName p | p <- interface n]

allPortLinks :: Graph -> [String]
allPortLinks g = concatMap portLinks (nodes g)

portLinks :: Node -> [String]
portLinks n = [ "link id" ++ (show (fromMaybe 0 (identity n))) ++ portName p ++ " id" ++ (show i) ++ s
              | p <- interface n
              , isJust (portConn p)
              , let (i, s) = fromJust (portConn p)]

bindVariables :: Graph -> [String]
bindVariables g = ["(" ++ intercalate "," (portNames n) ++ ")" | n <- nodes g]

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
  let g = head args
  Just gr <- (decode . FingBS.pack) <$> readFile g
  writeFile "model.hs" (generateFile gr)
