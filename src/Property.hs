{-# LANGUAGE TypeOperators #-}

module Property where

import CP hiding ((===), (==>))
import qualified CP ((===), (==>))
import GCM
import Test.QuickCheck

(.=)   = (CP.===)
(===>) = (CP.==>)

-- ----------------------------------------------------------------------------

-- Artificial test component
testGCM :: GCM (Port Int, Port Int)
testGCM = do
  pa <- createPort
  pb <- createPort

  component $ do
    a <- value pa
    b <- value pb
    assert $    (a .= 0) ===> (b .= 0)
    assert $ nt (a .= 0) ===> (b .< a)
 
  return (pa, pb)

-- Artificial specification
specGCM :: (Int, Int) -> Property
specGCM (a, b) =
      ((a == 0) ==> (b == 0)) .&&.
      ((a /= 0) ==> (b < a))

-- | Specify a property which should hold for all terms in @a@ when interpreting
-- the `GCM` component with.
--
-- TODO 
--   Type signature is a bit bad: Would like (Port a, Port b, ...) 
invariant :: GCM (Port a) -> (a -> Bool) -> a -> Property
invariant gcm pred x = pred x ==> eval gcm (undefined :: Env)

-- | If the solver backend produces values @x :: a@ when given the compiled
-- `GCM` program, then the property @prop@ should hold for these values.
--
-- TODO Type signature is a bit arbitrary
sound :: GCM a -> (a -> Property) -> Property
sound gcm prop = undefined

{-
 
   I'd like to state things such as

    invariant testGCM $ \(a, b) -> 
      a  = 0 ==> b = 0 /\
      a <> 0 ==> b < a

    sound testGCM $ \(a, b) -> 
      ...

   or using some corresponding syntax, and test
   that this holds for all terms (a, b) when I interpret the GCM
   component after substituting the relevant variables (i.e. ports) with
   these literals. 
   
   To each createPort should be a unique integer identifier which we can track
   down in the CPExps and substitute out.

-}

-- ----------------------------------------------------------------------------

data Env

-- | Interpret `GCM` components.
--
-- TODO 
--   Again, bad: would like some interface for `Port a`
eval :: GCM (Port a) -> Env -> Bool
eval gcm env = undefined


