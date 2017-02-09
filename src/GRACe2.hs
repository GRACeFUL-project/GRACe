{- Experimental shallow implementation on top of GCM.

   The goal is to get rid of the monads and produce a language which looks
   a bit more declarative.

   The expressions in the constraint system lend the entire setup used by
   Emil Axelsson and Koen Claessen in "Using Circular Programs for Higher-Order
   Syntax", http://www.cse.chalmers.se/~emax/documents/axelsson2013using.pdf.
-}
module GRACe2 where

import GL
import Compile0

-- Axelsson + Claessen thing
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- TODO Finish this part up. Extend GExp and use in constraints.

type Name = Integer

data GExp
  = Var Name 
  | Lam Name GExp
  | App GExp GExp

app :: GExp -> GExp -> GExp
app = App

lam :: (GExp -> GExp) -> GExp
lam f = Lam n body
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
maxBV (Var _)   = bot
maxBV (App f a) = maxBV f \/ maxBV a
maxBV (Lam n _) = n

-- GRACe thing
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A monoid instance for the type alias is not possible, sadly. Also
-- Another reason to get rid of GCM once we have established a working
-- embedding on top of it.
type GRACePrg = GCM ()

-- | An idea for how to make nicer `declare` syntax maybe?
class Declarable a where
  create :: GCM a

instance (CPType a) => Declarable (Port a) where
  create = createPort

instance (Declrarable a, Declarable b) => Declarable (a, b) where
  create = do
    a <- createPort
    b <- createPort
    return (a, b)

-- The easiest way to declare ports. We have the chance to rename these now.
declare :: (Declarable a) => (a -> GRACePrg) -> GRACePrg
declare f = create >>= f 

-- Composition
(<>) :: GRACePrg -> GRACePrg -> GRACePrg
(<>) = (>>)

{-
  These things are inherited from GRACe but should receive their own
  implementations, if we have use of them

  * output
  * link
  * set
-}

runGrace2 :: GRACePrg -> IO ()
runGrace2 = runGCM

-- Deliverable example redone
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rain :: Float -> Port Float -> GRACePrg
rain amount port =
  set port amount

pump :: Float
     -> Port Float
     -> Port Float
     -> GRACePrg
pump maxCap inPort outPort =
  error $ unlines 
    [ "constraints here"
    , ""
    , "inPort === outPort"           -- get rid of value
    , "inPort `inRange` (0, maxCap)" -- get rid of lit
    ]

runoffArea :: Float
           -> Port Float
           -> Port Float
           -> Port Float
           -> GRACePrg
runoffArea cap inflow outlet overflow = 
  error "constraints here"

-- Declare syntax slightly worse if there were not very few of them
example :: GRACePrg
example =
  declare $ \(rainOut, pumpOut) ->
       rain 5 rainOut
    <> pump 7 rainOut pumpOut
    <> output pumpOut "pumpOut"

