-- TODO:
--  * Finish this part up. Extend GExp and use in constraints.
--  * QuickCheck properties...
{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             DeriveDataTypeable #-}

import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data

type Name = Integer

data GExp
  = Var  Name 
  | Dec  Name GExp
  | Seq  GExp GExp
  | Less GExp GExp
  | Eql  GExp GExp
  | Lit  Int
  | Empty          -- Kind of ugly, doesn't need to be here
  deriving (Data, Typeable, Show)

instance Monoid GExp where
  mempty = Empty
  mappend = Seq

-- TODO: Float decs and links as far as possible here?
dec :: (GExp -> GExp) -> GExp
dec f = Dec n body
  where
    body = f (Var n)
    n    = prime (maxBV body)

-- Get rid of multiple `dec`s
class Vindaloo a where
  declare :: (a -> GExp) -> GExp

instance Vindaloo GExp where
  declare = dec

instance Vindaloo (GExp, GExp) where
  declare f = dec $ \v -> dec $ curry f v

bot :: Name
bot = 0

prime :: Name -> Name 
prime = succ

(\/) :: Name -> Name -> Name
(\/) = max

maxBV :: GExp -> Name 
maxBV (Var _)    = bot
maxBV (Lit _)    = bot
maxBV Empty      = bot
maxBV (Seq a b)  = maxBV a \/ maxBV b
maxBV (Less a b) = maxBV a \/ maxBV b
maxBV (Eql a b)  = maxBV a \/ maxBV b
maxBV (Dec n _)  = n

codegen :: GExp -> String
codegen Empty        = ""
codegen (Var x)      = vname x
codegen (Dec x body) = "var " ++ vname x ++ "\n" ++ codegen body
codegen (Less l r)   = codegen l ++ "<"  ++ codegen r
codegen (Eql l r)    = codegen l ++ "==" ++ codegen r
codegen (Seq x y)    = codegen x ++ "\n" ++ codegen y
codegen (Lit i)      = show i

vname :: Name -> String
vname i = "v" ++ show i

-- TODO: Maybe make this a smart constructor which
-- floats links and (===) as high as possible?
(<>) :: GExp -> GExp -> GExp
(Dec n body) <> x = declare $ \(Var n') -> replace n n' body <> x
x <> (Dec n body) = declare $ \(Var n') -> x <> replace n n' body
x <> y = Seq x y

infixr 2 <>

replace :: Name -> Name -> GExp -> GExp
replace n n' = transform (replace' n n')
  where
    replace' n n' (Var x)
      | x == n    = Var n'
      | otherwise = Var n
    replace' n n' x = x

class a `In` b where
  emb :: a -> b 

instance a `In` a where
  emb = id

instance Int `In` GExp where
  emb = Lit

(===) :: (a `In` GExp, b `In` GExp) => a -> b -> GExp
x === y = Eql (emb x) (emb y)

(<:) :: (a `In` GExp, b `In` GExp) => a -> b -> GExp
x <:  y = Less (emb x) (emb y)

-- Examples

pump :: Int -> GExp -> GExp -> GExp
pump c i o =
     i <:  c
  <> i === o

rain :: Int -> GExp -> GExp
rain r p = p === r

example :: GExp -> GExp
example x =
  declare $ \port ->
     rain 5 port
  <> pump 7 port x
