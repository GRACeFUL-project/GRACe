module Sign (Sign(..))
  where
import CP
import qualified Interfaces.MZASTBase as HZ

-- * A Sign type to define and reason about CLDs
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Sign = M   -- minus
          | Z   -- zero
          | P   -- plus
          | Q   -- ambiguous
          deriving (Eq, Ord)  -- ordering of constructors is important for Ord

instance CPType Sign where
  typeDec = const (++ " -2..2")
  hzType = const (HZ.Range (HZ.IConst $ -2) (HZ.IConst 2))
  hzConst x = HZ.IConst $ case x of
    M -> -1
    Z -> 0
    P -> 1
    Q -> 2

instance Num Sign where
  x + y = case (x, y) of
    (Z, b) -> b
    (a, Z) -> a
    (P, P) -> P
    (M, M) -> M
    (P, M) -> Q
    (M, P) -> Q
    (Q, _) -> Q
    (_, Q) -> Q
  fromInteger x | x == -1   = M
                | x ==  0   = Z
                | x ==  1   = P
                | otherwise = Q

instance Show Sign where
  show x = case x of
    Z -> "0"
    P -> "1"
    M -> "-1"
    Q -> "2"
