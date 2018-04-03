import Primes (isPrime, primes)
import Combs (combsworo)
import Data.List (nub, sort)
import System.IO

-- Problem 60: Prime pair sets
-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
-- primes and concatenating them in any order the result will always be
-- prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The
-- sum of these four primes, 792, represents the lowest sum for a set of four
-- primes with this property.
-- Find the lowest sum for a set of five primes for which any two primes
-- concatenate to produce another prime.
appendNum :: Int -> Int -> Int
appendNum n 0 = n
appendNum 0 m = m
appendNum n m = 10 * (appendNum n (m `div` 10)) + m `mod` 10

memoize :: (Int -> b) -> (Int -> b)
memoize f = (map f [0 ..] !!)

-- isPrimeMemo :: Int -> Bool
-- isPrimeMemo = memoize isPrime
isPrimePair :: Int -> Int -> Bool
isPrimePair p1 p2 = isPrime (appendNum p1 p2) && isPrime (appendNum p2 p1)

-- are the ith and jth primes prime pairs?
isPrimePairIdx :: [Int] -> Bool
isPrimePairIdx [i,j] =
  isPrimePair (primes !! i)
              (primes !! j)

isPrimePairIdxA :: Int -> Int -> Bool
isPrimePairIdxA i j =
  isPrimePair (primes !! i)
              (primes !! j)

-- combs2Memo :: Int -> [[Int]]
-- combswMemo n = memoize (combsworo [0 .. n])
primePairs :: Int -> [[Int]]
primePairs n = filter isPrimePairIdx (combsworo [0 .. n] 2)

allPrimePairIdxs :: [Int] -> Bool
allPrimePairIdxs xs =
  all (\[i,j] ->
         isPrimePairIdxA (xs !! i)
                         (xs !! j))
      (combsworo [0 .. length xs - 1] 2)

filterOrdL g [] = []
filterOrdL g (y:ys) =
  if g y
     then (y : (filterOrdL g ys))
     else []

-- an ordered version of filter, which assumes all true elements are consecutive
filterOrd :: (a -> Bool) -> [a] -> [a]
filterOrd p [] = []
filterOrd p (x:xs) =
  if p x
     then (x : (filterOrdL p xs))
     else filterOrd p xs

-- (all (isPrimePairIdx (head xs)) ys)
-- xxs are lists of prime paths of length 2, xs a path of any length
-- filter (all (isPrimePairIdxA (last xs)))
primePaths :: [[Int]] -> [Int] -> [[Int]]
primePaths xxs xs =
  map ((++) xs . tail)
      ((filter (\pr ->
                  all (isPrimePairIdxA (last pr))
                      (tail (reverse xs))) .
        filter ((== (last xs)) . head)) xxs)

primePathsOld :: [[Int]] -> [Int] -> [[Int]]
primePathsOld xxs xs =
  map ((++) xs . tail)
      ((filter (\pr ->
                  all (isPrimePairIdxA (last pr))
                      (tail (reverse xs))) .
        filter ((== (last xs)) . head)) xxs)

primePathsNext :: [[Int]] -> [[Int]] -> [[Int]]
primePathsNext pp2 pps = concat (map (primePaths pp2) pps)

primePathsNextOld
  :: [[Int]] -> [[Int]] -> [[Int]]
primePathsNextOld pp2 pps = concat (map (primePathsOld pp2) pps)

iterPrimePaths :: Int -> [[[Int]]]
iterPrimePaths n = iterate (primePathsNext pp) pp
  where pp = primePairs n

-- [5,691,750,867,1050]
p60 :: Int
p60 = (sum . map (primes !!) . head) ((iterPrimePaths 1100) !! 3)

main :: IO ()
main = (putStr . show) p60