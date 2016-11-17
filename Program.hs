{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Program where
import Control.Monad

-- Generic programs, parameterised over some instruction set
data Program instr a where
    Return :: a -> Program instr a
    (:>>=) :: Program instr a -> (a -> Program instr b) -> Program instr b
    Instr  :: instr a -> Program instr a

-- Obvious instance
instance Monad (Program instr) where
    return = Return
    (>>=)  = (:>>=)

-- Obvious instance
instance Applicative (Program instr) where
    pure = return
    (<*>) = ap

-- Obvious instance
instance Functor (Program instr) where
    fmap = liftM

-- Interpret
interpret :: (Monad m) => (forall b. instr b -> m b) -> Program instr a -> m a
interpret interp (Return a) = return a
interpret interp (m :>>= f) = interpret interp m >>= (interpret interp . f)
interpret interp (Instr  i) = interp i
