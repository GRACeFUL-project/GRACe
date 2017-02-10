-- TODO:
--  * Finish this part up. Extend GExp and use in constraints.
--  * QuickCheck properties...
{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}

import Control.Monad.State
import Data.Maybe
import Data.List

type Name = Integer

data GExp
  = Var  Name 
  | Dec  Name GExp
  | Seq  GExp GExp
  | Less GExp GExp
  | Eql  GExp GExp
  | Lit  Int
  | Empty          -- Kind of ugly, doesn't need to be here

instance Monoid GExp where
  mempty = Empty
  mappend = Seq

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

compile :: GExp -> String
compile = codegen . alphaRename

codegen :: GExp -> String
codegen Empty        = ""
codegen (Var x)      = vname x
codegen (Dec x body) = "var " ++ vname x ++ "\n" ++ codegen body
codegen (Less l r)   = codegen l ++ "<"  ++ codegen r
codegen (Eql l r)    = codegen l ++ "==" ++ codegen r
codegen (Seq x y)    = codegen x ++ "\n" ++ codegen y
codegen (Lit i)      = show i

alphaRename :: GExp -> GExp
alphaRename exp = evalState (helper [] exp) 0
  where
    substitute sub x = fromMaybe x $ lookup x sub

    helper _   Empty        = return Empty
    helper sub (Var x)      = return (Var $ substitute sub x)
    helper sub (Dec x body) = do
      x' <- get
      modify succ
      Dec x' <$>  helper ((x,x'):sub) body
    helper sub (Less l r)   = Less <$> (helper sub l) <*> (helper sub r)
    helper sub (Eql l r)    = Eql  <$> (helper sub l) <*> (helper sub r)
    helper sub (Seq l r)    = Seq  <$> (helper sub l) <*> (helper sub r)
    helper sub (Lit i)      = return (Lit i)

vname :: Name -> String
vname i = "v" ++ show i

(<>) :: GExp -> GExp -> GExp
(<>) = Seq

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
