{-# LANGUAGE DeriveGeneric
  , OverloadedStrings
  , TypeApplications
  , DeriveAnyClass
#-}
module GRACeGraph where
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

-- | An adjacency list representation of graphs
data Graph = Graph { nodes :: [Node] }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Nodes in the graph
data Node = Node { identity   :: Maybe Int   -- * What ID does this node have?
                 , name       :: String      -- * What is the name of the GL component?
                 , parameters :: [Parameter] -- * What are the parameters of this node?
                 , interface  :: [Interface] -- * What is the interface of this node?
                 }
  deriving (Generic, Show, Eq)

nodeOptions :: Options
nodeOptions = defaultOptions {omitNothingFields = True}

-- | Obvious instances
instance ToJSON Node where
  toJSON = genericToJSON nodeOptions

-- | Obvious instances
instance FromJSON Node where
  parseJSON  = genericParseJSON  nodeOptions

data Parameter = Parameter { parameterName  :: String
                           , parameterType  :: String
                           , parameterValue :: Maybe PrimTypeValue
                           }
  deriving (Generic, Show, Eq)

fixParameter :: Parameter -> Parameter
fixParameter p@(Parameter n t1 (Just t2)) =
  case (t1, t2) of
    ("Float", IntV x) -> Parameter n t1 (Just (FloatV (fromIntegral x)))
    _ -> p
fixParameter param = param

parameterOptions :: Options
parameterOptions = defaultOptions { fieldLabelModifier = (map toLower) . (drop $ length ("parameter" :: String)), omitNothingFields = True }

instance ToJSON Parameter where
  toJSON = genericToJSON parameterOptions

instance FromJSON Parameter where
  parseJSON  = genericParseJSON  parameterOptions

-- | The primitive types in `GRACe`
{-
data PrimType = FloatT | IntT | StringT | BoolT
  deriving (Generic, Show, Eq)

primTypeOptions :: Options
primTypeOptions = defaultOptions { constructorTagModifier = init }

instance ToJSON PrimType where
  toJSON = genericToJSON primTypeOptions

instance FromJSON PrimType where
  parseJSON  = genericParseJSON  primTypeOptions
-}
data PrimTypeValue = FloatV Float | IntV Int | StringV String | BoolV Bool
                   | NullV
  deriving (Generic, Show, Eq)

-- | PrimTypeValue are represented in JSON as just String/Bool/Number
instance ToJSON PrimTypeValue where
  toJSON (FloatV f)  = Number $ fromRational (toRational f)
  toJSON (IntV i)    = Number $ fromInteger (toInteger i)
  toJSON (StringV s) = String (T.pack s)
  toJSON (BoolV b)   = Bool b
  toJSON NullV       = Null

-- | Derived from the above instance
instance FromJSON PrimTypeValue where
  parseJSON (String s) = return $ StringV (T.unpack s)
  parseJSON (Bool b)   = return $ BoolV b
  parseJSON (Number s) = return $ either FloatV IntV $ floatingOrInteger s
  parseJSON (Null)     = return NullV
  parseJSON _          = fail "Does not comform to interface"

-- | Fix the prefixes
data Interface = Interface { interfaceName       :: String
                           , interfaceType       :: String
                           , interfaceConnection :: Maybe (Int, String, Maybe Int)
                           }
  deriving (Generic, Show, Eq)

interfaceOptions :: Options
interfaceOptions = defaultOptions { fieldLabelModifier = (map toLower) . (drop (length ("interface" :: String))), omitNothingFields = True }

instance ToJSON Interface where
  toJSON = genericToJSON interfaceOptions

instance FromJSON Interface where
  parseJSON  = genericParseJSON  interfaceOptions

example = Graph
  [ Node
     (Just 1)
     "pump"
     [ Parameter "capacity" "Float" (Just (FloatV 5))
     ]
     [ Interface "inflow"  "Float"  (Just (3, "outlet", Nothing))
     , Interface "outflow" "Float"  Nothing
     ]
  , Node
     (Just 2)
     "rain"
     [ Parameter "amount"   "Float" (Just (FloatV 10))
     ]
     [ Interface "rainfall" "Float" (Just (3, "inflow", Nothing))
     ]
  , Node
     (Just 3)
     "runoffArea"
     [ Parameter "capacity" "Float" (Just (FloatV 5))
     ]
     [ Interface "inflow"   "Float" Nothing
     , Interface "outlet"   "Float" Nothing
     , Interface "overflow" "Float" Nothing
     ]
  ]

pPrintPTV :: PrimTypeValue -> String
pPrintPTV (FloatV f)  = show f
pPrintPTV (IntV i)    = show i
pPrintPTV (StringV s) = show s
pPrintPTV (BoolV b)   = show b
