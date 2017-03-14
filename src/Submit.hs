{-# LANGUAGE GADTs #-}
-- Working towards creating GRACe programs from JSON
 
import GL
import Compile0
import Library
import GRACeGraph
import Types
import Service
import qualified Data.Map as Map
import Data.List
import Data.Maybe

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

witness :: Type a -> Maybe a
witness (Pair x y) = (,) <$> (witness x) <*> (witness y)
witness _ = Nothing

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
