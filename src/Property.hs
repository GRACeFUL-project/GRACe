{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}

module Property where

import           CP               hiding ((===), (==>))
import qualified CP               ((===), (==>))
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           GCM
import           Test.QuickCheck

-- TODO Remove, not supposed to construct properties in here.
(.=)   = (CP.===)
(===>) = (CP.==>)

-- * `Port` plumbing
-- ----------------------------------------------------------------------------

-- | `Expose` allows us to extract a list of `Variable` identifiers from some
-- type composed by `Port`s.
class Expose a where
  type T a
  expose :: a -> [T a]

instance Expose (Port a) where
  type T (Port a) = Variable a
  expose (Port v) = [v]

instance Expose (Port a, Port a) where
  type T (Port a, Port a) = Variable a
  expose (Port v, Port w) = [v, w]

instance Expose (Port a, Port a, Port a) where
  type T (Port a, Port a, Port a) = Variable a
  expose (Port v, Port w, Port x) = [v, w, x]

-- | `Flatten` is just there to sort out the type signature of `invariant` and
-- `sound`.
type family Flatten a
type instance Flatten (Port a)                 = a
type instance Flatten [Port a]                 = [a]
type instance Flatten (Port a, Port b)         = (a, b)
type instance Flatten (Port a, Port b, Port c) = (a, b, c)

-- * Specifying GCM properties
-- ----------------------------------------------------------------------------

-- | Specify a property which should hold for all terms in @a@ when interpreting
-- the `GCM` component with.
invariant :: Expose a => GCM a -> (Flatten a -> Bool) -> Flatten a -> Property
invariant gcm pred x = pred x ==> eval gcm (undefined :: Env a)

-- | If the solver backend produces values @x :: a@ when given the compiled
-- `GCM` program, then the property @prop@ should hold for these values.
--
-- TODO
--   * Type signature is a bit arbitrary
--   * Not sure how this would work
sound :: Expose a => GCM a -> (Flatten a -> Property) -> Flatten a -> Property
sound gcm prop = undefined

-- * Interpretation
-- ----------------------------------------------------------------------------

-- TODO
--   Interpreting CPExps in a meaninful way is tricky:
--     * To use equality, we need a more general type for the interpreter.
--     * A more general type for the interpreter means we cannot mix all
--       expressions; they have different restrictions from typeclasses.

-- | Environment for `GCM` interpretation. The `Variable` indexes into the
-- environment, and points to a `CPExp` literal.
type Env a = Map (Variable a) (CPExp a)

-- | Construct an empty environment (i.e. just find out what variables
-- exist in the program).
mkEnv :: Expose a => a -> Env a
mkEnv = undefined

-- | Interpret `GCM` components in some /fully specified/ environment. Returns
-- the product of all assertions.
eval :: Expose a => GCM a -> Env a -> Bool
eval gcm env = undefined

-- * Tests
-- ----------------------------------------------------------------------------

True `implies` False = False
_    `implies` _     = True

-- | Artifical test component.
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

-- | Boolean specification for 'testGCM'.
specGCM :: (Int, Int) -> Bool
specGCM (a, b) =
    (a == 0) `implies` (b == 0) &&
    (a /= 0) `implies` (b < a)

-- | Test property for 'testGCM'.
testProp :: (Int, Int) -> Property
testProp = invariant testGCM specGCM

