-- TODO Finish this part up. Extend GExp and use in constraints.

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
codegen (Dec x body) = "var v" ++ show x ++ ";\n" ++ codegen body
codegen (Less l r)   = codegen l ++ "<" ++ codegen r
codegen (Eql l r)    = codegen l ++ "==" ++ codegen r
codegen (Seq x y)    = codegen x ++ ";\n" ++ codegen y
codegen (Lit i)      = show i

(<>) = Seq

pump :: Int -> GExp -> GExp -> GExp
pump c i o = Less i (Lit c) <> Eql i o

rain :: Int -> GExp -> GExp
rain r p = Eql p (Lit r)

example :: GExp -> GExp
example x =
  dec $ \port ->
     pump 7 port x
  <> rain 5 port
