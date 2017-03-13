{-# LANGUAGE GADTs, RankNTypes #-}
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
-- Ack         :  The code is based on Ideas.Service.Types module developed
--                by the Ideas team. The code can be found here:
--                https://github.com/ideas-edu/ideas
--
-----------------------------------------------------------------------------

module Service where

import GL
import Compile0
import Types

import Data.Aeson hiding (String)

-- Evaluator
------------------------

type Dec m a = forall t . Type t -> a -> m t

type Enc m a = TypedValue -> m a

evil :: Dec GCM b -> Enc GCM c -> b -> TypedValue -> GCM c
evil dec enc b = rec
 where
   rec tv@(val ::: tp) = case tp of
         -- uncurry function if possible
       t1 :-> t2 :-> t3 ->
           rec (uncurry val ::: Pair t1 t2 :-> t3)
       t1 :-> t2 -> do
           a <- dec t1 b
           rec (val a ::: t2)
         -- perform IO
       GCM t -> do
           a <- val
           rec (a ::: t)
       _ -> enc tv


jsonDec :: Monad m => Dec m Value
jsonDec t v = case t of
    Const Float  -> unsafeFromJSON v
    Const Int    -> unsafeFromJSON v
    Const String -> unsafeFromJSON v
    Tag _ t'     -> jsonDec t' v
  where
    unsafeFromJSON x = case fromJSON x of
        Success y -> return y
        _         -> fail "decoding failed"

jsonEnc :: Monad m => Enc m Value
jsonEnc (val ::: t) = return $ case t of
    Const Float -> toJSON val

raar :: (IsTyped a, IsTyped t) => a -> TypedValue -> GCM t
raar = evil castT fromTyped  

instance ToJSON TypedValue where
    toJSON (val ::: t) = case t of
        Const Float -> toJSON val

