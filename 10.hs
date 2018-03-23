import Data.List.Ordered (minus, union, unionAll)

-- Problem 10: Summation of primes
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
-- primes1 :: [Int]
-- primes1 = sieve [2 ..]
--   where sieve (p:xs) = p : sieve [(x)|x <- xs,x `mod` p /= 0]
primes =
  2 :
  3 :
  minus [5,7 ..]
        (unionAll [[p * p,p * p + 2 * p ..]|p <- tail primes])

-- sumPrimes 2000000
sumPrimes :: Int -> Int
sumPrimes n = sum (takeWhile (< n) primes)