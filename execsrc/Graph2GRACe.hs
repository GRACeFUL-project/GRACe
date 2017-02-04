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
 -  * `FilledOutInterface` needs a better json instance
 -}

data Graph = Graph { nodes :: [FilledOutNode] }
  deriving (Generic, Show, Eq)
instance ToJSON Graph where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Graph

data FilledOutNode = FilledOutNode { identity   :: Maybe Int
                                   , name       :: String
                                   , parameters :: [FilledOutParameter]
                                   , interface  :: [FilledOutInterface]
                                   }
  deriving (Generic, Show, Eq)
instance ToJSON FilledOutNode where
  toEncoding = genericToEncoding $ defaultOptions {omitNothingFields = True}
instance FromJSON FilledOutNode where
  parseJSON  = genericParseJSON  $ defaultOptions {omitNothingFields = True}

-- | Fix the prefixes
data FilledOutParameter = FilledOutParameter { parameterName  :: String
                                             , parameterType  :: PrimType
                                             , parameterValue :: PrimTypeValue
                                             }
  deriving (Generic, Show, Eq)
instance ToJSON FilledOutParameter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FilledOutParameter

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

instance FromJSON PrimTypeValue where
  parseJSON (String s) = return $ StringV (T.unpack s)
  parseJSON (Bool b)   = return $ BoolV b
  parseJSON (Number s) = return $ either FloatV IntV $ floatingOrInteger s 
  parseJSON _          = fail "Does not comform to interface"

-- | Fix the prefixes
data FilledOutInterface = FilledOutInterface { portName :: String
                                             , portType :: String
                                             , portConn :: Maybe (Int, String)
                                             }
  deriving (Generic, Show, Eq)

instance ToJSON FilledOutInterface where
  toEncoding = genericToEncoding $ defaultOptions {omitNothingFields = True}

instance FromJSON FilledOutInterface where
  parseJSON  = genericParseJSON $ defaultOptions {omitNothingFields = True}

example = Graph
  [ FilledOutNode
     (Just 1)
     "pump"
     [ FilledOutParameter "capacity" FloatT (FloatV 5) ]
     [ FilledOutInterface "inflow" "flow" (Just (3, "outlet"))
     , FilledOutInterface "outflow" "flow" Nothing
     ]
  , FilledOutNode
     (Just 2)
     "rain"
     [ FilledOutParameter "amount" FloatT (FloatV 10) ]
     [ FilledOutInterface "rainfall" "flow" (Just (3, "inflow")) ]
  , FilledOutNode
     (Just 3)
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
imports g = nub $ ["import " ++ capf (name n) | n <- nodes g]
  where
    capf [] = []
    capf (x:xs) = (toUpper x):xs

components :: Graph -> [String]
components g = [name n ++ " " ++ intercalate " " (prettyArgs n) | n <- nodes g]

prettyArgs :: FilledOutNode -> [String]
prettyArgs n = [pPrintPTV (parameterValue p) | p <- parameters n]

allPortNames :: Graph -> [String]
allPortNames g = concatMap portNames (nodes g)

portNames :: FilledOutNode -> [String]
portNames n = [ "id" ++ show (fromMaybe 0 (identity n)) ++ portName p | p <- interface n]

allPortLinks :: Graph -> [String]
allPortLinks g = concatMap portLinks (nodes g)

portLinks :: FilledOutNode -> [String]
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
