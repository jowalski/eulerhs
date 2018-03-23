import Data.List

-- Problem 5: Smallest multiple
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
-- What is the smallest number that is evenly divisible by all of the numbers
-- from 1 to 20?
-- prob5 :: Int -> Int -> [Int]
-- prob5 testmax max = [m|m <- [2520 .. testmax],[n|n <- [1..max],m `mod` n == 0] == [1..max]]
primes :: [Int]
primes = sieve [2 ..]
  where sieve (p:xs) = p : sieve [(x)|x <- xs,x `mod` p /= 0]

-- the number of times p divides n evenly
nDiv :: Int -> Int -> Int
nDiv n p
  | n `mod` p == 0 = 1 + nDiv (n `div` p) p
  | otherwise = 0

-- prime factorization
factors :: Int -> [(Int,Int)]
factors n
  | fs == [] = [(n,1)]
  | otherwise = fs
  where fs =
          [(p,nDiv n p)|p <- takeWhile (< (n `div` 2) + 1) primes,nDiv n p > 0]

-- the maximum factors of a set of numbers
maxFactors :: [Int] -> [(Int,Int)]
maxFactors xs =
  map last
      (groupBy (\(x,_) (y,_) -> x == y)
               (sort [(fxs)|fs <- xs,fxs <- factors fs]))

-- (factorsToNum . maxFactors) [1..10] => 2520
-- (factorsToNum . maxFactors) [1..20] => 232792560
factorsToNum :: [(Int,Int)] -> Int
factorsToNum [] = 1
factorsToNum ((x,y):xs) = (x ^ y) * factorsToNum xs