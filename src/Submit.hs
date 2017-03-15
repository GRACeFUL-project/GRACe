{-# LANGUAGE GADTs, RecordWildCards, Strict #-}
{- | Working towards creating GRACe programs from JSON

  TODOs: 

  * Change the structure of the lookup functions:

    Go from 
      Id -> Id -> GCM TypedValue 
      item_name -> parameter_name -> TV parameter representation 
    to  
      Id -> Id -> Id -> GCM TypedValue
      node_id -> item_name -> parameter_name -> TV parameter representation
      
    At this point, we cannot use library components twice, and we cannot name 
    ports in two different components as "inflow", for instance, or one would
    overwrite the other.

  * Review whether or not there should be so many GCMs in 
    the type signatures (lookups, for instance)

  * A general cleanup before merging this into wherever its supposed to go
    would be the decent thing to do.

-}
module Submit where
 
import           GL
import           Compile0
import           Library
import           GRACeGraph
import           Types
import           Service
import           Control.Monad
import           Data.Aeson hiding (String)
import qualified Data.ByteString.Lazy.Char8 as BS 
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.List
import           Data.Maybe
import           Utils

import           Debug.Trace

-- may end up changing types of these, don't know if storing ports as TVs is a good idea
makeNodes :: Library -> [Node] -> GCM([Map.Map String TypedValue])
makeNodes lib nodes = mapM (makeNode lib) nodes

makeNode :: Library -> Node -> GCM (Map.Map String TypedValue)
makeNode lib node = case libraryLookup lib node of
  Nothing -> fail "Component was not found in library."
  Just tv -> return Map.empty

libraryLookup :: Library -> Node -> Maybe TypedValue
libraryLookup lib node = case findid lib node of
  Just item -> Just (f item)
  Nothing -> Nothing
  where findid lib node = find (\x -> (itemId x == name node)) (items lib)

-- Parameter values, order of parameters specified by TypedValue
params :: TypedValue -> Node -> [Maybe PrimTypeValue]
params (f ::: t) node = map (pval . findParam (parameters node)) $ paramNames t where
  findParam :: [Parameter] -> String -> Maybe Parameter
  findParam params pname = find (\p -> (parameterName p) == pname) params
  pval :: Maybe Parameter -> Maybe PrimTypeValue
  pval (Just par) = parameterValue par
  pval _        = Nothing
  paramNames :: Type a -> [String]
  paramNames ((Tag n t1) :-> t2) = n : (paramNames t2)
  paramNames _                 = []
 
ggDec :: Monad m => Dec m (Maybe PrimTypeValue)
ggDec t v = case t of
    Const Float  -> exfval v
    Const Int    -> exival v
    Const String -> exsval v
    Tag _ t'     -> ggDec t' v
    where
        exfval (Just (FloatV f)) = return f
        exfval _ = fail "type mismatch"
        exsval (Just (StringV s)) = return s
        exsfval _ = fail "type mismatch"
        exival (Just (IntV i)) = return i
        exifval _ = fail "type mismatch"

-- uses type witness to get around type annotation
newfoo :: (IsTyped a) => Maybe PrimTypeValue -> TypedValue -> a -> GCM a
newfoo p tv x = evil ggDec fromTyped p tv

-- Attempts toward constructing type witness from TypedValue
skipToNext :: Type (a -> b) -> Type b
skipToNext (_ :-> tnext) = tnext

-- couldn't get this to work, type related errors
-- skipToEnd (_ :-> tnext) = skipToEnd tnext
-- skipToEnd tlast = tlast

-- | Generate a type witness from a `Type`. 
witness :: Type a -> Maybe a
witness (Pair x y) = (,) <$> (witness x) <*> (witness y)
witness _ = Nothing --- TODO cases

-- example stuff
library :: Library
library = Library "crud"
    [ Item "rain" $
         rain ::: "amount" # tFloat .-> tGCM ("rainfall" # tPort tFloat)

    , Item "pump" $
        pump ::: "capacity" # tFloat.-> tGCM (tPair ("inflowP" # tPort tFloat)
                                                    ("outflowP" # tPort tFloat))
    , Item "runoff area" $
        runoffArea ::: "storage capacity" # tFloat .-> tGCM (tTuple3 ("inflow" # tPort tFloat)
                                                                     ("outlet" # tPort tFloat)
                                                                     ("overflow" # tPort tFloat))
    ]

-- * Operations on `TypedValue`s
-- ----------------------------------------------------------------------------

type Id = String

lookupG :: (Show k, Show v, Ord k) => k -> Map k v -> GCM v
lookupG k m = 
  case Map.lookup k m of
    Nothing -> fail $ "- failed to look up the id " ++ show k ++ " in " ++ 
                      Map.showTree m
    Just v  -> return v

-- | Extract the identifier of all function arguments/component parameters of a
-- `TypedValue`, if any.
idents :: TypedValue -> [Id]
idents (_ ::: t) = go t
  where
    go :: Type a -> [Id]
    go (Tag n _ :-> ts) = n : go ts 
    go _                = []


untag :: Type a -> Type a
untag (Tag _ t) = t
untag t         = t

-- | Perform a function application under `TypedValue` representation, for a
-- single argument.
apply1 :: TypedValue -> TypedValue -> GCM TypedValue
apply1 (f ::: (t1 :-> t2)) (x ::: t3) = 
  case equal t3 t1 of
    Nothing -> fail "- equal failed in apply1"
    Just g -> return $ f (g x) ::: t2

-- | Perform function application under `TypedValue` representation, if 
-- possible.
apply :: TypedValue -> [TypedValue] -> GCM TypedValue
apply = foldM apply1

-- | Perform a function application inside an `Item`.
applyItem :: (Id -> GCM TypedValue) -> Item -> GCM Item
applyItem f (Item n tv) = do
  tvs <- apply tv =<< mapM f (idents tv)
  return (Item n tvs)

-- | Perform application on the entire `Library`.
applyLibrary :: Library -> (Id -> Id -> GCM TypedValue) -> GCM Library
applyLibrary (Library n is) f = 
  Library n <$> mapM (\x -> applyItem (f (itemId x)) x) is

-- | Document.
-- TODO Instance for n-tuples with Iso
put :: Id                      -- ^ Document
    -> TypedValue              -- ^ Document
    -> Map Id TypedValue       -- ^ Document
    -> GCM (Map Id TypedValue) 
put cid tv@(x ::: t) m = 
  case t of 
    Tag n (Port' p) -> return $ Map.insert (n ++ cid) (x ::: Port' p) m
    Pair a b        -> Map.union <$> put cid (fst x ::: a) m
                                 <*> put cid (snd x ::: b) m
    Iso iso (Pair a b) -> Map.union <$> put cid (fst (to iso x) ::: a) m
                                    <*> put cid (snd (to iso x) ::: b) m
    _ -> fail $ "- unable to split the Type of value " ++ show tv

-- | Document
putItem :: Map Id TypedValue       -- ^ Document
        -> Item                    -- ^ Document
        -> GCM (Map Id TypedValue)
putItem m (Item _ (x ::: GCM t)) = do
  x1 <- x
  put "" (x1 ::: t) m
putItem _ y = fail $ "- tried to putItem non-GCM value " ++ show y 

linkTV :: TypedValue  -- ^ Document
       -> TypedValue  -- ^ Document
       -> GCM ()
linkTV (x ::: t1@(Port' _)) (y ::: t@(Port' _)) =
  case equal t1 t of
    Nothing -> fail "- unable to link"
    Just f  -> link (f x) y

-- | Document
link2 :: Map Id TypedValue -- ^ Document
      -> Id                -- ^ Document
      -> Id                -- ^ Document
      -> GCM ()
link2 m i j = join $ linkTV <$> lookupG i m <*> lookupG j m

-- Convenience function.
lookat :: (Show k, Show v, Ord k) => Map k (Map k v) -> k -> k -> GCM v
lookat m k1 k2 = 
  lookupG k2 =<< lookupG k1 m

-- | Generate a `TypedValue` from an identifier tag and a `PrimTypeValue`.
fromPrimType :: String -> PrimTypeValue -> TypedValue
fromPrimType name ptv =
  case ptv of 
    FloatV f  -> f ::: name # tFloat
    IntV i    -> i ::: name # tInt
    StringV s -> s ::: name # tString
    BoolV b   -> b ::: name # tBool

-- | Extract all `Node` parameters.
-- TODO This is not a very nice way to leave it.
paramTVs :: [Node] -> GCM (Map Id (Map Id TypedValue))
paramTVs ns = Map.fromList <$> mapM fromNode ns
  where
    fromNode :: Node -> GCM (Id, Map Id TypedValue)
    fromNode Node {..} = 
      case identity of
        Nothing    -> fail "failure in paramTVs.fromNode"
        Just ident ->
          (\x -> (name, Map.fromList x)) <$> 
            mapM (fromParam . fixParameter) parameters
   
    fromParam :: Parameter -> GCM (Id, TypedValue)
    fromParam Parameter {..} =
      case parameterValue of 
        Nothing  -> fail "failure in paramTVs.fromParam"
        Just ptv -> return (parameterName, fromPrimType parameterName ptv)

-- | Extract all `Node` links.
getLinks :: [Node] -> [(Id, Id)]
getLinks = concatMap fromNode 
  where
    fromNode :: Node -> [(Id, Id)]
    fromNode Node {..} = 
      case identity of 
        Nothing    -> fail "failure in getLinks.fromNode"
        Just ident -> mapMaybe (fromInterface (show ident)) interface

    fromInterface :: Id -> Interface -> Maybe (Id, Id)
    fromInterface from Interface {..} =
      case interfaceConnection of
        Nothing          -> Nothing
        Just (node, tag) -> Just (interfaceName, tag)
        {-Just (node, tag) -> Just (interfaceName ++ from, tag ++ show node)-}
        
-- | Construct a `GCM` program from a list of `Node`s and a `Library`.
mkGCM :: [Node] -> Library -> GCM [()]
mkGCM ns l = do
  m   <- paramTVs ns
  lib <- applyLibrary l (lookat m)
  gs  <- foldM putItem Map.empty (items lib)
  mapM (uncurry (link2 gs)) (getLinks ns)

-- | Run a `Library` using a list of `Node`s.
{-runLibrary :: [Node] -> Library -> IO ()-}
runLibrary ns = compileGCM . mkGCM ns

testLibrary = do
  Just gr <- (decode . BS.pack) <$> readFile "../example.json"
  putStrLn $ runLibrary (nodes gr) library

-- ----------------------------------------------------------------------------

rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

pump :: Float -> GCM (Port Float, Port Float)
pump maxCap = do
  inPort  <- createPort
  outPort <- createPort

  component $ do
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap)

  return (inPort, outPort)

runoffArea :: Float -> GCM (Port Float, Port Float, Port Float)
runoffArea cap = do
  inflow <- createPort
  outlet <- createPort
  overflow <- createPort

  component $ do
    currentStored <- createLVar

    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    sto <- value currentStored

    assert $ sto === inf - out - ovf
    assert $ sto `inRange` (0, lit cap)
    assert $ (ovf .> 0) ==> (sto === lit cap)
    assert $ ovf .>= 0

  return (inflow, outlet, overflow)

example :: GCM ()
example = do
  (inflowP, outflowP) <- pump 5
  (inflowS, outletS, overflowS) <- runoffArea 5
  rainflow <- rain 10

  link inflowP outletS
  link inflowS rainflow

  output overflowS "Overflow"

example2 :: GCM ()
example2 = do
  x <- createPort :: GCM (Port Float)
  rainGCM <- newfoo (Just (FloatV 10.0)) (f $ head $ items library) x
  --rainGCM <- evil ggDec fromTyped (Just (FloatV 10.0)) (f $ head $ items library) :: GCM (Port Float)

  -- fails because we don't have IsTyped instance for 3-tuples
  --runoffGCM <- evil ggDec fromTyped (Just (FloatV 5.0)) (f $ last $ items library) :: GCM (Port Float, Port Float, Port Float)

  -- ghc compiles this but then if compileGCM is called it fails with a Prelude.undefined exception tracing back to line 309 in Types.hs
  --pumpGCM <- evil ggDec fromTyped (Just (FloatV 5.0)) (f $ head $ tail $ items library) :: GCM (Port Float, Port Float)

  --link (fst pumpGCM) rainGCM
  --link (fst pumpGCM) (snd3 runoffGCM)
  --link (fst3 runoffGCM) rainGCM
  return ()

-- TODO: We need to deal with all of our tuples in a nicer way
fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x
snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y
