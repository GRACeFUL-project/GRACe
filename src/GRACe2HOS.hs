-- TODO Finish this part up. Extend GExp and use in constraints.
{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts
#-}

type Name = Integer

data GExp
  = Var  Name 
  | Dec  Name GExp
  | Seq  GExp GExp
  | Less GExp GExp
  | Eql  GExp GExp
  | Lit  Int

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
maxBV (Seq a b)  = maxBV a \/ maxBV b
maxBV (Less a b) = maxBV a \/ maxBV b
maxBV (Eql a b)  = maxBV a \/ maxBV b
maxBV (Dec n _)  = n

codegen :: GExp -> String
codegen (Var x)      = "v" ++ show x
codegen (Dec x body) = "var v" ++ show x ++ "\n" ++ codegen body
codegen (Less l r)   = codegen l ++ "<" ++ codegen r
codegen (Eql l r)    = codegen l ++ "==" ++ codegen r
codegen (Seq x y)    = codegen x ++ "\n" ++ codegen y
codegen (Lit i)      = show i

(<>) = Seq

pump :: Int -> GExp -> GExp -> GExp
pump c i o = i <: c <> i === o

rain :: Int -> GExp -> GExp
rain r p = p === r

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

example :: GExp -> GExp
example x =
  declare $ \port ->
     rain 5 port
  <> pump 7 port x
