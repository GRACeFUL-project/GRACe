-- TODO:
--  * Finish this part up. Extend GExp and use in constraints.
--  * QuickCheck properties...
{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             DeriveDataTypeable,
             ScopedTypeVariables #-}

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
  | BinOp Op GExp GExp
  | Lit  Int
  deriving (Data, Typeable)

data Op = Eql
        | Less
        deriving (Typeable, Data, Show)

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
maxBV (Var _)       = bot
maxBV (Lit _)       = bot
maxBV (BinOp _ _ _) = bot
maxBV (Seq a b)     = maxBV a \/ maxBV b
maxBV (Dec n _)     = n

(<>) :: GExp -> GExp -> GExp
(Dec n body) <> x = declare $ \(Var n') -> replace n n' body <> x
x <> (Dec n body) = declare $ \(Var n') -> x <> replace n n' body
x <> y            = Seq x y

infixr 0 <>

replace :: Name -> Name -> GExp -> GExp
replace n n' = transform (replace' n n')
  where
    replace' n n' (Var x)
      | x == n    = Var n'
      | otherwise = Var x
    replace' n n' x = x

codegen :: GExp -> String
codegen (Var x)        = vname x
codegen (Dec x body)   = "var " ++ vname x ++ "\n" ++ codegen body
codegen (BinOp op l r) = codegen l ++ opStr op ++ codegen r
codegen (Seq x y)      = codegen x ++ "\n" ++ codegen y
codegen (Lit i)        = show i

opStr :: Op -> String
opStr Less = "<"
opStr Eql  = "=="

vname :: Name -> String
vname i = "v" ++ show i

class a `In` b where
  emb :: a -> b 

instance a `In` a where
  emb = id

instance Int `In` GExp where
  emb = Lit

(===) :: (a `In` GExp, b `In` GExp) => a -> b -> GExp
x === y = BinOp Eql (emb x) (emb y)

(<:) :: (a `In` GExp, b `In` GExp) => a -> b -> GExp
x <:  y = BinOp Less (emb x) (emb y)

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
     rain 5  port
  <> pump 7  port x
