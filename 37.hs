import Data.List.Ordered (minus, union, unionAll)

-- Problem 37: Truncatable primes
-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain
-- prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
-- right to left: 3797, 379, 37, and 3.
-- Find the sum of the only eleven primes that are both truncatable from left
-- to right and right to left.
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 1 = False
isPrime k = null [(x)|x <- [2 .. isqrt k],k `mod` x == 0]

primes :: [Int]
primes =
  2 :
  3 :
  minus [5,7 ..]
        (unionAll [[p * p,p * p + 2 * p ..]|p <- tail primes])

numToDigits :: Int -> [Int]
numToDigits = reverse . digsr

digsr :: Int -> [Int]
digsr 0 = []
digsr n = (n `mod` 10) : (digsr (n `div` 10))

digitsToNum :: [Int] -> Int
digitsToNum ns = ltn ns 0
  where ltn [] val = val
        ltn ns' val =
          ltn (tail ns')
              ((val * 10) + head ns')

truncatableFromRight :: Int -> Bool
truncatableFromRight n =
  all (isPrime . digitsToNum)
      (map truncn posns)
  where ns = numToDigits n
        truncn m = take m ns
        posns = [1 .. length ns]

truncatableFromLeft :: Int -> Bool
truncatableFromLeft n =
  all (isPrime . digitsToNum)
      (map truncn posns)
  where ns = numToDigits n
        truncn m = drop m ns
        posns = reverse [1 .. length ns]

truncfl :: [Int] -> Int -> [Int]
truncfl ns m = drop m ns

truncfr :: [Int] -> Int -> [Int]
truncfr ns m = take m ns

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)

smallToLargeTruncs :: [Int] -> [[Int]]
smallToLargeTruncs ns = interleave fromLeft fromRight
  where fromLeft =
          map (truncfl ns)
              (reverse [1 .. length ns - 1])
        fromRight =
          map (truncfr ns)
              [1 .. length ns]

truncatable :: Int -> Bool
truncatable n =
  all (isPrime . digitsToNum)
      (smallToLargeTruncs (numToDigits n))

p37 :: Int
p37 = sum (take 11 (filter truncatable (dropWhile (< 10) primes)))