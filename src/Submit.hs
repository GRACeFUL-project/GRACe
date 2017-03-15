{-# LANGUAGE GADTs #-}
-- Working towards creating GRACe programs from JSON
module Submit where
 
import           GL
import           Compile0
import           Library
import           GRACeGraph
import           Types
import           Service
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List
import           Data.Maybe

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
         rain ::: "amount" # tFloat .-> tGCM (tPort $ "rainfall" # tFloat)
    , Item "pump" $
        pump ::: "capacity" # tFloat.-> tGCM (tPair (tPort $ "inflow" # tFloat)
                                                    (tPort $ "outflow" # tFloat))
    , Item "runoff area" $
        runoffArea ::: "storage capacity" # tFloat .-> tGCM (tTuple3 (tPort $ "inflow"   # tFloat)
                                                                     (tPort $ "outlet"   # tFloat)
                                                                     (tPort $ "overflow" # tFloat))
    ]

-- * Operations on `TypedValue`s
-- ----------------------------------------------------------------------------

type Id = String

-- | Extract the identifier of all function arguments/component parameters of a
-- `TypedValue`, if any.
idents :: TypedValue -> [Id]
idents (_ ::: t) = go t
  where
    go :: Type a -> [Id]
    go (Tag n _ :-> ts) = n : go ts 
    go _                = []

-- | Perform a function under `TypedValue` representation.
apply1 :: TypedValue -> TypedValue -> Maybe TypedValue
apply1 (f ::: (t1 :-> t2)) (x ::: t3) = (\g -> f (g x) ::: t2) <$> equal t3 t1
apply1 _                   _          = Nothing

-- | Perform function application under `TypedValue` representation, if 
-- possible.
apply :: TypedValue -> [TypedValue] -> Maybe TypedValue
apply = foldM apply1

-- | Self-explanatory code.
applyItem :: (Id -> Maybe TypedValue) -> Item -> Maybe Item
applyItem f (Item n tv) = do
  maybe_tvs <- mapM f (idents tv)
  maybe_tv <- apply tv maybe_tvs 
  return (Item n maybe_tv)

-- | Perform application on the entire `Library`.
applyLibrary :: Library -> (Id -> Id -> Maybe TypedValue) -> Maybe Library
applyLibrary (Library n is) f = Library n <$> go f is
  where
    go f []     = return []
    go f (x:xs) = do
      y  <- applyItem (f (itemId x)) x
      ys <- go f xs
      return (y:ys)

-- | Insert a bunch of concrete values represented as `TypedValue`s into
-- some map.
put :: TypedValue -> Map Id TypedValue -> Maybe (Map Id TypedValue)
put tv@(x ::: t) m = 
  case t of 
    Tag n Unit      -> return $ Map.insert n (x ::: Unit) m
    Tag n (Const c) -> return $ Map.insert n (x ::: Const c) m
    Pair a b        -> Map.union <$> put (fst x ::: a) m <*> put (snd x ::: b) m
    _               -> Nothing

-- | The typechecker is trying really hard to prevent this.
link2 :: Id -> Id -> Map Id TypedValue -> Maybe (GCM ())
link2 i j m = error "not really working out"
  -- do
  --   (x ::: Port' _) <- Map.lookup i m
  --   (y ::: _) <- Map.lookup j m
  --   return $ link x y

{-
-- Faux parameter maps for items
rainF   = Map.fromList [ ("amount", 5.0 ::: "amount" # tFloat) ]
pumpF   = Map.fromList [ ("capacity", 6.0 ::: "capacity" # tFloat) ]
runoffF = Map.fromList [ ("storage capacity", 7.0 ::: "storage capacity" # tFloat) ]

-- Faux library map
faux :: Map Id (Map Id TypedValue)
faux = Map.fromList 
  [ ("rain", rainF)
  , ("pump", pumpF)
  , ("runoff area", runoffF)
  ]
-}

-- | Function for lookups.
lookat :: Ord k => Map k (Map k v) -> k -> k -> Maybe v
lookat m k1 k2 = Map.lookup k2 =<< Map.lookup k1 m

-- | Running.
runLibrary :: Library -> Map Id (Map Id TypedValue) -> Maybe (GCM ())
runLibrary l m = do
  lib <- applyLibrary l (lookat m) 
  fail "TODO: Parse connections -> run series of link:s" 

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
