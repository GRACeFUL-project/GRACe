{-# LANGUAGE ExistentialQuantification, RankNTypes, GADTs, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- Copyright 2017, GRACeFUL project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alexg@chalmers.se
-- Stability   :  experimental
-- Portability :  portable (depends on ghc)
--
-- A small data type for expressing components.
--
-----------------------------------------------------------------------------

module Library
    ( module Types
    , Library(..)
    , Item(..), item
    , insert, combine, combineList
      -- Re export
    , module GCM
    , module CP
    , module Sign
    ) where

import GCM hiding (Item)
import CP
import Sign
import Types
import Utils

import Data.Aeson
import qualified Data.Text as T
import qualified Data.List as L

type Id  = String
type URL = String

data Library = Library
    { libraryId :: Id
    , items     :: [Item]
    } deriving Show

instance ToJSON Library where
    toJSON (Library n is) = object
        [ "library" .= toJSONList is]

data Item = Item
    { itemId      :: Id
    , annotations :: [String]
    , f           :: TypedValue
    } deriving Show

item :: Id -> TypedValue -> Item
item n = Item n []

instance ToJSON Item where
    toJSON (Item n as (f ::: t)) = object $
        [ "name"        .= n
        , "parameters"  .= parameters t
        , "interface"   .= ports [] t
        ]
        ++ jsonAnnotations as

jsonAnnotations :: [String] -> [(T.Text, Value)]
jsonAnnotations as = map (\(k,v) -> (T.pack k) .= (drop 2 v)) kvs
  where kvs = map (break (== ':')) as

parameters :: Type a -> Value
parameters = toJSONList . rec
  where
    rec :: Type a -> [Value]
    rec tp = case tp of
        t@(Tag n t1) :-> t2 -> tagParam t : rec t2
        _                   -> []

-- We want to annotate the actual types, not the types they are isomorphic to.
prettyShow :: Type a -> String
prettyShow t = case t of
  Iso _ (t1 :|: Unit) -> "Maybe " ++ prettyShow t1
  Iso _ tInt          -> "Sign"
  List t1             -> "[" ++ prettyShow t1 ++ "]"
  Port' t1            -> "Port " ++ "(" ++ prettyShow t1 ++ ")"
  tp@(Pair t1 t2)          -> "(" ++ L.intercalate "," (prettyCollect tp) ++ ")"
  _     -> show t
  where prettyCollect :: Type t -> [String]
        prettyCollect (Pair t1 t2) = prettyCollect t1 ++ prettyCollect t2
        prettyCollect t = [prettyShow t]

tagParam :: Type a -> Value
tagParam (Tag n t) = object
    [ "name"        .= n
    , "type"        .= prettyShow t ]
tagParam _ = Null

ports :: [String] -> Type a -> [Value]
ports as tp = case tp of
    -- base
    Tag n x -> case x of
        Port' _ -> [tagPort as tp]
        Pair (Port' _) (Port' _)        -> [tagPort as tp]
        List (Port' _)                  -> [tagPort as tp]
        List (Pair (Port' _) (Port' _)) -> [tagPort as tp]
        Tag _ _                         -> ports (as ++ [n]) x
        _                               -> []
    -- recurse
    GCM t      -> ports as t
    List t     -> ports as t
    Pair t1 t2 -> ports as t1 ++ ports as t2
    _ :-> t2   -> ports as t2
    Iso _ t    -> ports as t
    _          -> []

tagPort :: [String] -> Type a -> Value
tagPort as (Tag n t) = object $
    [ "name"         .= n
    , "type"         .= prettyShow t
    , "description"  .= n
    , "imgURL"       .= T.concat ["./data/interfaces/", T.pack n, ".png"]
    , "label"        .= head n]
    ++ jsonAnnotations as
tagPort _ _ = Null

insert :: [Item] -> Library -> Library
insert its (Library n is) = Library n (is ++ its)

combine :: String -> Library -> Library -> Library
combine n l1 l2 = Library n (items l1 ++ items l2)

combineList :: String -> [Library] -> Library
combineList n libs = Library n $ foldl (++) [] (map items libs)
