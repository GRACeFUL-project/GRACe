module QuickCheckTesting where
import Test.QuickCheck
import Data.Ratio

someFun :: [Int] -> Int
someFun [] = 0
someFun xs = head xs

symmetry :: [a] -> [a]
symmetry = reverse

symRel :: Eq a => [a] -> [a] -> Bool
symRel xs ys  =  xs == reverse ys

-- test: any two symmetric sequences give the same result under someFun

propSym :: Eq b => ([a]->b) -> [a] -> Bool
propSym f xs  =   f xs == f (symmetry xs)

propSym' :: (Eq a, Eq b) => ([a]->b) -> [a] -> [a] -> Property
propSym' f xs ys =  symRel xs ys ==> f xs == f ys

propSym'' ::  (Arbitrary a, Show a, Eq b) =>
              ([a] -> b) -> Property
propSym'' f = forAll (genSymPairs arbitrary) $ \(xs, ys) ->
                f xs == f ys
propSym3 ::  (Arbitrary a, Show a, Eq b) =>
              ([a] -> b) -> Property
propSym3 f = forAllShrink (genSymPairs arbitrary) shrinkSymPairs $ \(xs, ys) ->
                f xs == f ys

shrinkSymPairs :: Arbitrary a => ([a], [a]) -> [([a], [a])]
shrinkSymPairs (xs, _ys) = [ (xs, symmetry xs) | xs <- shrink xs ]


genSymPairs :: Gen a -> Gen ([a], [a])
genSymPairs g = do
  xs <- listOf g
  return (xs, symmetry xs)

main = do
  quickCheck (propSym'  someFun)
  quickCheck (propSym'' someFun)
  quickCheck (propSym3  someFun)

----------------------------------------------------------------

affine :: [Rational] -> [Rational] -> Bool
--  exists a, b. map (\x -> a + b*x) xs == ys
affine []   []   = True
affine [_x] [_y] = True
affine xs ys = and (zipWith (\x y -> a + b*x == y) xs ys)
  where (a, b) = findCoeff xs ys

findCoeff :: [Rational] -> [Rational] -> (Rational, Rational)
findCoeff xs ys = error "TODO"

----------------

localOrderPreserving :: Ord a => [a] -> [a] -> Bool
localOrderPreserving xs ys  =  localCompare xs == localCompare ys

localCompare xs = zipWith compare xs (tail xs)

countSummits :: Ord a => [a] -> Int
countSummits xs = countMatches [GT,LT] (localCompare xs)

-- assume no overlap
countMatches :: Eq a => [a] -> [a] -> Int
countMatches pat xs | length xs < length pat = 0
                    | otherwise =  (if start == pat then 1 else 0)
                                   + countMatches pat rest
  where (start, rest) = splitAt (length pat) xs


prop :: (Ord a, Eq b) => ([a]->b) -> [a] -> [a] -> Property
prop f xs ys =  localOrderPreserving xs ys  ==>  f xs == f ys

myProp :: [Int] -> Property
myProp xs = forAll arbitrary $ \a ->
            forAll arbitrary $ \(Positive b) ->
--            collect b $
            countSummits xs  ==  countSummits (map (\x -> a + b*x) xs)

propMono :: ([Int]->Int) -> [Int] -> [Int] -> Property
propMono = prop

test = do
  quickCheck myProp
  -- quickCheck (propMono countSummits)   -- too rarely localOrderPreserving

----------------
{-

Notes: Constraint solving may come in handy for generation of test
cases satisfying certain invariants. See the work of Arnaud Gotlieb:
http://people.rennes.inria.fr/Arnaud.Gotlieb/

-}
