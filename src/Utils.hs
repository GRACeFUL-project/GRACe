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

module Utils where

import Control.Arrow
import qualified Control.Category as C

-----------------------------------------------------------
-- Type class BiArrow

infix 1 <->

-- |Type class for bi-directional arrows. @<->@ should be used instead of
-- @arr@ from the arrow interface. Minimal complete definition: @<->@.
class Arrow arr => BiArrow arr where
   (<->) :: (a -> b) -> (b -> a) -> arr a b
   (!->) :: (a -> b) -> arr a b
   (<-!) :: (b -> a) -> arr a b
   -- default definitions
   (!->) f = f <-> errBiArrow
   (<-!) f = errBiArrow <-> f

errBiArrow :: a
errBiArrow = error "BiArrow: not bi-directional"

-----------------------------------------------------------
-- Type class BiFunctor

class BiFunctor f where
   biMap     :: (a -> c) -> (b -> d) -> f a b -> f c d
   mapFirst  :: (a -> b) -> f a c -> f b c
   mapSecond :: (b -> c) -> f a b -> f a c
   -- default definitions
   mapFirst  = flip biMap id
   mapSecond = biMap id

instance BiFunctor Either where
   biMap f g = either (Left . f) (Right . g)

instance BiFunctor (,) where
  biMap f g (a, b) = (f a, g b)

mapBoth :: BiFunctor f => (a -> b) -> f a a -> f b b
mapBoth f = biMap f f

copy :: BiArrow arr => arr a (a, a)
copy = (\a -> (a, a)) <-> fst

identity :: C.Category f => f a a
identity = C.id

----------------------------------------------------------------------------------
-- Isomorphisms (embedding-projection pairs)

-- to ep . from ep == id
data Isomorphism a b = EP { from :: a -> b, to :: b -> a }

instance C.Category Isomorphism where
   id    = id <-> id
   f . g = (from f . from g) <-> (to g . to f)

instance Arrow Isomorphism where
   arr     = (!->)
   first   = (*** identity)
   second  = (identity ***)
   p *** q = from p *** from q <-> to p *** to q
   f &&& g = copy >>> (f *** g)

instance BiArrow Isomorphism where
   (<->) = EP
