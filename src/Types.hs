{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, FlexibleContexts #-}
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

module Types
    ( -- * Types
      Type(..), Const(..), TypedValue(..)
    , Equal(..), equalM
     -- * Constructing types
    , tInt, tBool, tString, tFloat, tSign
    , tUnit, tPair, tTuple3, tTuple4, tTuple5, tMaybe, tList
    , tError, (.->), tIO, tPort, tGCM, (#)
     -- * Evaluating and searching a typed value
    , eval, findValuesOfType
     -- * From and to typed values
    , IsTyped(..), cast, castT
    ) where

import Utils
import GCM
import CP
import Sign

import Control.Arrow ((***))
import qualified Control.Category as C
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->
infixr 5 :|:

data TypedValue = forall a . a ::: Type a

data Type t where
    -- Type isomorphisms (for defining type synonyms)
    Iso   :: Isomorphism t1 t2 -> Type t1 -> Type t2
    -- Function type
    (:->) :: Type t1 -> Type t2 -> Type (t1 -> t2)
    -- Input/output
    IO    :: Type t -> Type (IO t)
    GCM   :: Type t -> Type (GCM t)
    Port' :: CPType t => Type t -> Type (Port t)
    -- Special annotations
    Tag   :: String -> Type t -> Type t
    -- Type constructors
    List  :: Type t  -> Type [t]
    Pair  :: Type t1 -> Type t2 -> Type (t1, t2)
    (:|:) :: Type t1 -> Type t2 -> Type (Either t1 t2)
    Unit  :: Type ()
    -- Type constants
    Const :: Const t -> Type t

data Const t where
    Bool   :: Const Bool
    Int    :: Const Int
    Float  :: Const Float
    String :: Const String
    Sign   :: Const Sign

instance Show (Type t) where
    show (Iso _ t)      = show t
    show (t1 :-> t2)    = show t1 +++ "->" +++ show t2
    show (IO t)         = "IO" +++ parens t
    show (GCM t)        = "GCM" +++ parens t
    show (Port' t)      = "Port" +++ parens t
    show (Tag s t)      = s +++ ":" +++ show t
    show t@(Pair _ _)   = showTuple t
    show (t1 :|: t2)    = show t1 +++ "|" +++ show t2
    show (List t)       = "[" ++ show t ++ "]"
    show Unit           = "()"
    show (Const c)      = show c

parens :: Show a => a -> String
parens x = "(" ++ show x ++ ")"

(+++) :: String -> String -> String
x +++ y = x ++ " " ++ y

instance Show TypedValue where
    show (val ::: tp) = case tp of
        Iso iso t  -> show (to iso val ::: t)
        _ :-> _    -> "<<function>>"
        IO _       -> "<<io>>"
        GCM _      -> "<<gcm>>"
        Port' n    -> "port_" ++ show n
        Tag _ t    -> show (val ::: t)
        List t     -> showAsList (map (show . (::: t)) val)
        Pair t1 t2 -> "(" ++ show (fst val ::: t1) ++
                      "," ++ show (snd val ::: t2) ++ ")"
        t1 :|: t2  -> either (show . (::: t1)) (show . (::: t2)) val
        Unit       -> "()"
        Const t    -> showConst val t

showAsList :: [String] -> String
showAsList xs = "[" ++ intercalate "," xs ++ "]"

showConst :: t -> Const t -> String
showConst val t = case t of
    Bool   -> map toLower (show val)
    Int    -> show val
    Float  -> show val
    String -> val

instance Show (Const t) where
    show Bool   = "Bool"
    show Int    = "Int"
    show Float  = "Float"
    show String = "String"

showTuple :: Type t -> String
showTuple tp = "(" ++ intercalate ", " (collect tp) ++ ")"
  where
    collect :: Type t -> [String]
    collect (Pair t1 t2) = collect t1 ++ collect t2
    collect (Iso _ t)    = collect t
    collect t            = [show t]

---------------------------------------------------------------

tError :: Type t -> Type (Either String t)
tError = (:|:) tString

tIO :: Type t -> Type (IO t)
tIO = IO

tGCM :: Type t -> Type (GCM t)
tGCM = GCM

tPort :: CPType t => Type t -> Type (Port t)
tPort t = Port' t 

(#) :: String -> Type t -> Type t
(#) = Tag

infixr 6 #

infixr 5 .->

(.->) :: Type t1 -> Type t2 -> Type (t1 -> t2)
(.->) = (:->)

tMaybe :: Type t -> Type (Maybe t)
tMaybe t = Iso (f <-> g) (t :|: Unit)
    where
      f = either Just (const Nothing)
      g = maybe (Right ()) Left

tList :: Type t -> Type [t]
tList = List

tUnit :: Type ()
tUnit = Unit

tPair :: Type t1 -> Type t2 -> Type (t1, t2)
tPair = Pair

tString :: Type String
tString = Const String

tBool :: Type Bool
tBool = Const Bool

tInt :: Type Int
tInt = Const Int

tFloat :: Type Float
tFloat = Const Float

tSign :: Type Sign
tSign = Const Sign

tTuple3 :: Type t1 -> Type t2 -> Type t3 -> Type (t1, t2, t3)
tTuple3 t1 t2 t3 = Iso (f <-> g) (Pair t1 (Pair t2 t3))
    where
      f (a, (b, c)) = (a, b, c)
      g (a, b, c)   = (a, (b, c))

tTuple4 :: Type t1 -> Type t2 -> Type t3 -> Type t4 -> Type (t1, t2, t3, t4)
tTuple4 t1 t2 t3 t4 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 t4)))
    where
      f (a, (b, (c, d))) = (a, b, c, d)
      g (a, b, c, d)     = (a, (b, (c, d)))

tTuple5 :: Type t1 -> Type t2 -> Type t3 -> Type t4 -> Type t5 -> Type (t1, t2, t3, t4, t5)
tTuple5 t1 t2 t3 t4 t5 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 (Pair t4 t5))))
    where
      f (a, (b, (c, (d, e)))) = (a, b, c, d, e)
      g (a, b, c, d, e)       = (a, (b, (c, (d, e))))

-----------------------------------------------------------------------------
-- Type equality

class Equal f where
   equal :: f a -> f b -> Maybe (a -> b)

equalM :: Monad m => Type t1 -> Type t2 -> m (t1 -> t2)
equalM t1 t2 = maybe (fail msg) return (equal t1 t2)
  where
    msg = "Types not equal: " ++ show t1 ++ " and " ++ show t2

instance Equal Type where
    equal (Iso p a)  t2         = fmap (. to p) (equal a t2)
    equal t1         (Iso p b)  = fmap (from p .) (equal t1 b)
    equal (a :-> b)  (c :-> d)  = liftM2 (\f g h -> g . h . f)
                                         (equal c a) (equal b d)
    equal (Pair a b) (Pair c d) = liftM2 (***) (equal a c) (equal b d)
    equal (a :|: b)  (c :|: d)  = liftM2 biMap (equal a c) (equal b d)
    equal (List a)   (List b)   = fmap map (equal a b)
    equal (Tag s1 a) t2         = equal a t2
    equal t1         (Tag s2 b) = equal t1 b
    equal Unit       Unit       = Just id
    equal (Const a)  (Const b)  = equal a b
    equal (Port' a)  (Port' b)  = fmap (\f -> fmap f) $ equal a b
    equal _          _          = Nothing

instance Equal Const where
    equal Int         Int       = Just id
    equal Bool        Bool      = Just id
    equal Float       Float     = Just id
    equal String      String    = Just id
    equal Sign        Sign      = Just id
    equal _           _         = Nothing

findValuesOfType :: Type t -> TypedValue -> [t]
findValuesOfType thisType = rec
 where
   rec tv@(a ::: tp) =
      case equal tp thisType of
         Just f  -> [f a]
         Nothing -> recDown tv

   recDown (a ::: tp) =
      case tp of
         Iso iso t  -> rec (to iso a ::: t)
         Tag _ t    -> rec (a ::: t)
         List t     -> concatMap (\b -> rec (b ::: t)) a
         Pair t1 t2 -> rec (fst a ::: t1) ++ rec (snd a ::: t2)
         t1 :|: t2  -> either (\b -> rec (b ::: t1)) (\b -> rec (b ::: t2)) a
         _          -> []

-- Evaluation of typed values
eval :: (IsTyped t, IsTyped a) => TypedValue -> a -> GCM t
eval tv x = rec tv 
  where
    rec tv@(val ::: t) = case t of
        Tag _ t'      -> rec (val ::: t')
        a :-> b :-> c -> rec (uncurry val ::: Pair a b :-> c)
        a :-> b       -> castT a x >>= \x' -> rec (val x' ::: b)
        GCM t         -> val >>= \a -> rec (a ::: t)
        _             -> fromTyped tv

-- Check type
castT :: (IsTyped a, Monad m) => Type t -> a -> m t
castT t x = equalM (typeOf x) t >>= \f -> return (f x)

cast :: (IsTyped a, IsTyped b, Monad m) => a -> m b
cast = castT (typeOf (undefined :: b))

-- Conversion to and from typed values
class IsTyped a where
    typeOf    :: a -> Type a
    toTyped   :: a -> TypedValue
    toTyped x  = x ::: typeOf x
    fromTyped :: Monad m => TypedValue -> m a

instance IsTyped Int where
    typeOf _ = tInt
    fromTyped (x ::: Const Int) = return x
    fromTyped _                 = fail errMsg

instance IsTyped Float where
    typeOf _ = tFloat
    fromTyped (x ::: Const Float) = return x
    fromTyped _                   = fail errMsg

instance IsTyped Sign where
    typeOf _ = tSign
    fromTyped (x ::: Const Sign) = return x
    fromTyped _                  = fail errMsg

instance {-# OVERLAPPING #-} IsTyped String where
    typeOf _ = tString
    fromTyped (x ::: Const String) = return x
    fromTyped _                    = fail errMsg

instance (CPType a, IsTyped a) => IsTyped (Port a) where
    typeOf (Port _) = tPort (typeOf (undefined :: a))
    fromTyped (x ::: t@(Port' _)) = do 
        f <- equalM t $ tPort (typeOf (undefined :: a))
        return (f x)
    fromTyped _ = fail errMsg

instance IsTyped Bool where
    typeOf _ = tBool
    fromTyped (x ::: Const Bool) = return x
    fromTyped _                  = fail errMsg

instance (IsTyped a, IsTyped b) => IsTyped (a, b) where
    typeOf (x, y) = tPair (typeOf x) (typeOf y)
    fromTyped (p ::: t@(Pair a b)) = do
        f <- equalM t $ tPair (typeOf (undefined :: a)) 
                              (typeOf (undefined :: b))
        return (f p)
    fromTyped _ = fail errMsg

instance IsTyped a => IsTyped [a] where
    typeOf _ = tList (typeOf (undefined :: a))
    fromTyped (xs ::: t@(List a)) = do
        f <- equalM t $ tList (typeOf (undefined :: a))
        return (f xs)
    fromTyped _ = fail errMsg

errMsg :: String
errMsg = "fromTyped failed"
