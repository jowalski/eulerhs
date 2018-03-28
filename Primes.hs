module Primes where

import Data.List.Ordered (minus, unionAll)

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