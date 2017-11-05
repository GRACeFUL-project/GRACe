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
    , insert, combine
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
    , description :: String
    , imgURL      :: URL
    , itemType    :: String
    , f           :: TypedValue
    } deriving Show

item :: Id -> TypedValue -> Item
item n = Item n "no comment" "" ""

instance ToJSON Item where
    toJSON (Item n c i l (f ::: t)) = object
        [ "name"        .= n
        , "parameters"  .= parameters t
        , "interface"   .= ports t
        , "description" .= c
        , "imgURL"      .= i
        , "itemType"    .= l
        ]

parameters :: Type a -> Value
parameters = toJSONList . rec
  where
    rec :: Type a -> [Value]
    rec tp = case tp of
        t@(Tag n t1) :-> t2 -> tagParam t : rec t2
        _                   -> []

tagParam :: Type a -> Value
tagParam (Tag n t) = object
    [ "name"        .= n
    , "type"        .= show t ]
tagParam _ = Null

ports :: Type a -> [Value]
ports tp = case tp of
    -- base
    Tag r (Tag iT (Tag oT (Tag n (Port' t)))) -> [tagPort tp]
    -- recurse
    Tag _ t    -> ports t
    GCM t      -> ports t
    List t     -> ports t
    Pair t1 t2 -> ports t1 ++ ports t2
    _ :-> t2   -> ports t2
    Iso _ t    -> ports t
    _          -> []

tagPort :: Type a -> Value
tagPort (Tag r (Tag iT (Tag oT (Tag n t)))) = object
    [ "name"         .= n
    , "type"         .= show t
    , "description"  .= n
    , "imgURL"       .= T.concat ["./data/interfaces/", T.pack n, ".png"]
    , "label"        .= head n
    , "rotation"     .= r
    , "incomingType" .= iT
    , "outgoingType" .= oT]
tagPort _ = Null

insert :: [Item] -> Library -> Library
insert its (Library n is) = Library n (is ++ its)

combine :: String -> Library -> Library -> Library
combine n (Library _ is) (Library _ js) = Library n (is ++ js)
