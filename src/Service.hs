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

import Types

type Id = String

data Service = S
   { serviceId         :: Id
   , description       :: String
   , serviceDeprecated :: Bool
   , serviceFunction   :: TypedValue
   }

instance Show Service where
   show = serviceId

makeService :: String -> String -> TypedValue -> Service
makeService s descr f =  S s descr False f

deprecate :: Service -> Service
deprecate s = s { serviceDeprecated = True }

-- Evaluator
------------------------

type Dec a = forall t . Type t -> a -> IO t

type Enc a = TypedValue -> IO a

data EvalResult c = EvalResult
   { inputValues :: [TypedValue]
   , outputValue :: TypedValue 
   , evalResult  :: c
   }

evalService :: Dec b -> Enc a -> Service -> b -> IO a
evalService dec enc srv b = do
   res <- eval dec enc b (serviceFunction srv)
   return (evalResult res)

eval :: Dec b -> Enc c -> b -> TypedValue -> IO (EvalResult c)
eval dec enc b = rec
 where
   rec tv@(val ::: tp) = case tp of
       -- handle exceptions
       Const String :|: t ->
           either fail (\a -> rec (a ::: t)) val
         -- uncurry function if possible
       t1 :-> t2 :-> t3 ->
           rec (uncurry val ::: Pair t1 t2 :-> t3)
       t1 :-> t2 -> do
           a   <- dec t1 b
           res <- rec (val a ::: t2)
           return res { inputValues = (a ::: t1) : inputValues res }
         -- perform IO
       IO t -> do
           a <- val
           rec (a ::: t)
       _ -> do
           c <- enc tv
           return $ EvalResult [] tv c



libraryS :: Service
libraryS = makeService "library"
   "List all the available components" $
   (\n -> ["pump", "rain"]) ::: tString .-> tList (tString)
