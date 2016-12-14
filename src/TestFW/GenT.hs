-- |
-- Most of the code is borrowed from
-- <http://haskell.1045720.n5.nabble.com/darcs-patch-GenT-monad-transformer-variant-of-Gen-QuickCheck-2-td3172136.html a mailing list discussion>.
-- Therefor, credits go to Paul Johnson and Felix Martini.

-- This code is an adaptation of: http://hackage.haskell.org/package/QuickCheck-GenT
module TestFW.GenT where

import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import qualified System.Random as Random
import Control.Monad.Trans
import Control.Monad

newtype GenT m a = GenT { unGenT :: QC.QCGen -> Int -> m a }

instance (Functor m) => Functor (GenT m) where
  fmap f m = GenT $ \r n -> fmap f $ unGenT m r n

instance (Monad m) => Monad (GenT m) where
  return a = GenT (\_ _ -> return a)
  m >>= k = GenT $ \r n -> do
    let (r1, r2) = Random.split r
    a <- unGenT m r1 n
    unGenT (k a) r2 n
  fail msg = GenT (\_ _ -> fail msg)

instance (Functor m, Monad m) => Applicative (GenT m) where
  pure = return
  (<*>) = ap

instance MonadTrans GenT where
  lift m = GenT (\_ _ -> m)

instance (MonadIO m) => MonadIO (GenT m) where
  liftIO = lift . liftIO

runGenT :: GenT m a -> QC.Gen (m a)
runGenT (GenT run) = QC.MkGen run

class (Applicative g, Monad g) => MonadGen g where
  liftGen :: QC.Gen a -> g a
  variant :: Integral n => n -> g a -> g a
  sized :: (Int -> g a) -> g a
  resize :: Int -> g a -> g a
  choose :: Random.Random a => (a, a) -> g a

instance (Applicative m, Monad m) => MonadGen (GenT m) where
  liftGen gen = GenT $ \r n -> return $ QC.unGen gen r n
  choose rng = GenT $ \r _ -> return $ fst $ Random.randomR rng r
  variant k (GenT g) = GenT $ \r n -> g (var k r) n
  sized f = GenT $ \r n -> let GenT g = f n in g r n
  resize n (GenT g) = GenT $ \r _ -> g r n

instance MonadGen QC.Gen where
  liftGen = id
  variant k (QC.MkGen g) = QC.MkGen $ \r n -> g (var k r) n
  sized f = QC.MkGen $ \r n -> let QC.MkGen g = f n in g r n
  resize n (QC.MkGen g) = QC.MkGen $ \r _ -> g r n
  choose range = QC.MkGen $ \r _ -> fst $ Random.randomR range r

-- |
-- Private variant-generating function.  Converts an integer into a chain
-- of (fst . split) and (snd . split) applications.  Every integer (including
-- negative ones) will give rise to a different random number generator in
-- log2 n steps.
var :: Integral n => n -> QC.QCGen -> QC.QCGen
var k =
  (if k == k' then id else var k') . (if even k then fst else snd) . Random.split
  where k' = k `div` 2
