{-# LANGUAGE GADTs, RecordWildCards, Strict #-}
{- | Creating GRACe programs from JSON

  TODOs:

  * Review whether or not there should be so many GCMs in
    the type signatures (lookups, for instance)

  * A general cleanup before merging this into wherever its supposed to go
    would be the decent thing to do.

-}
module Submit where

import           GCM
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
applyItem f (Item n c i tv) = do
  tvs <- apply tv =<< mapM f (idents tv)
  return (Item n c i tvs)

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
    Tag n (List (Port' p)) -> return $ Map.insert (n ++ cid) (x ::: List (Port' p)) m
    _ -> fail $ "- unable to split the Type of value " ++ show tv ++ " :: " ++ show t

-- | Document
putItem :: Map Id TypedValue       -- ^ Document
        -> Item                    -- ^ Document
        -> GCM (Map Id TypedValue)
putItem m (Item n _ _ (x ::: GCM t)) = do
  x1 <- x
  put n (x1 ::: t) m
putItem _ y = fail $ "- tried to putItem non-GCM value " ++ show y


-- | Document
link2 :: Map Id TypedValue -- ^ Document
      -> Id                -- ^ Document
      -> Id                -- ^ Document
      -> Maybe Int         -- ^ Offset
      -> GCM ()
link2 m i j offset = join $ linkTV <$> (at offset <$> lookupG i m) <*> (at offset <$> lookupG j m)
  where
    linkTV :: TypedValue -> TypedValue -> GCM ()
    linkTV (x ::: t1@(Port' _)) (y ::: t@(Port' _)) =
      case equal t1 t of
        Nothing -> fail "- unable to link"
        Just f  -> do
            output x i
            output y j
            link (f x) y
    linkTV (x ::: t1) (y ::: t2) =
      fail ("unable to link " ++ show t1 ++ show t2)

    -- If an offset is given, and the type is a list of ports, then index into
    -- the list using the offset
    at :: Maybe Int -> TypedValue -> TypedValue
    at offset tv = case offset of
      Just n -> case tv of
        (xs ::: List p@(Port' _)) -> (xs !! n) ::: p
        _ -> tv
      _ -> tv

-- Convenience function.
lookat :: (Show k, Show v, Ord k) => Map k (Map k v) -> k -> k -> GCM v
lookat m k1 k2 =
  lookupG k2 =<< lookupG k1 m

-- | Generate a `TypedValue` from an identifier tag, a type annotation,
--   and a `PrimTypeValue`.
fromPrimType :: String -> String -> PrimTypeValue -> TypedValue
fromPrimType name typ ptv =
  case (typ, ptv) of
    ("Float", FloatV f)  -> f ::: name # tFloat
    ("Int", IntV i)    -> i ::: name # tInt
    ("Sign", IntV i)    -> (fromInteger (toInteger i)) ::: name # tSign
    ("Maybe Sign", v)    -> case v of
      IntV i -> Just (fromInteger (toInteger i)) ::: name # tMaybe tSign
      NullV  -> Nothing ::: name # tMaybe tSign
      _      -> error "CRASH!"
    ("String", StringV s) -> s ::: name # tString
    ("Bool", BoolV b)   -> b ::: name # tBool
    (_,_) -> error "Types don't match"

-- | Extract all `Node` parameters.
-- TODO This is not a very nice way to leave it.
paramTVs :: Monad m => [Node] -> m (Map Id (Map Id TypedValue))
paramTVs ns = Map.fromList <$> mapM fromNode ns
  where
    fromNode :: Monad m => Node -> m (Id, Map Id TypedValue)
    fromNode Node {..} =
      case identity of
        Nothing    -> fail "failure in paramTVs.fromNode"
        Just ident ->
          (\x -> (show ident, Map.fromList x)) <$>
            mapM (fromParam . fixParameter) parameters

    fromParam :: Monad m => Parameter -> m (Id, TypedValue)
    fromParam Parameter {..} =
      case parameterValue of
        Nothing  -> case parameterType of
          "Maybe Sign" -> return (parameterName, fromPrimType parameterName parameterType NullV)
          _            -> fail ("failure in paramTVs.fromParam " ++ parameterName)
        Just ptv -> return (parameterName, fromPrimType parameterName parameterType ptv)

-- | Extract all `Node` links.
getLinks :: [Node] -> [(Id, Id, Maybe Int)]
getLinks = concatMap fromNode
  where
    fromNode :: Node -> [(Id, Id, Maybe Int)]
    fromNode Node {..} =
      case identity of
        Nothing    -> fail "failure in getLinks.fromNode"
        Just ident -> mapMaybe (fromInterface (show ident)) interface

    fromInterface :: Id -> Interface -> Maybe (Id, Id, Maybe Int)
    fromInterface from Interface {..} =
      case interfaceConnection of
        Nothing          -> Nothing
        Just (node, tag, offset) -> Just (interfaceName ++ from, tag ++ show node, offset)

-- | Construct a `Library` containing an `Item` instance from the given `Library`
--   for each `Node` in the given list.
nodeLibrary :: [Node] -> Library -> Library
nodeLibrary ns Library {..} = Library "" $ catMaybes $ map (nodeItem items) ns
  where
    nodeItem :: [Item] -> Node -> Maybe Item
    nodeItem is Node {..} = case (identity, find (\i -> itemId i == name) is) of
       (Just ident, Just it) -> Just (Item (show ident) (comment it) (icon it) (f it))
       _                     -> Nothing

-- | Construct a `GCM` program from a list of `Node`s and a `Library`.
mkGCM :: [Node] -> Library -> GCM ()
mkGCM ns l = do
  m   <- paramTVs ns
  lib <- applyLibrary (nodeLibrary ns l) (lookat m)
  gs  <- foldM putItem Map.empty (items lib)
  mapM_ (\(n, p, m) -> link2 gs n p m) (getLinks ns)
