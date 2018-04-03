module Primes where

import Data.List.Ordered (minus, unionAll)

divTimes :: Int -> Int -> Int
divTimes n d
  | n `mod` d /= 0 = 0
  | otherwise = 1 + divTimes (n `div` d) d

divTimesI :: Integer -> Integer -> Integer
divTimesI n d
  | n `mod` d /= 0 = 0
  | otherwise = 1 + divTimesI (n `div` d) d

pfs :: [Int] -> Int -> [(Int,Int)]
pfs ps n
  | n == 1 = []
  | otherwise =
    case divTimes n p of
      0 -> pfs (tail ps) n
      d ->
        (p,d) :
        (pfs (tail ps)
             (n `div` (p ^ d)))
  where p = head ps

primeFactors :: Int -> [(Int,Int)]
primeFactors = pfs primes

pfsI
  :: [Integer] -> Integer -> [(Integer,Integer)]
pfsI ps n
  | n == 1 = []
  | otherwise =
    case divTimesI n p of
      0 -> pfsI (tail ps) n
      d ->
        (p,d) :
        (pfsI (tail ps)
              (n `div` (p ^ d)))
  where p = head ps

primeFactorsI :: Integer -> [(Integer,Integer)]
primeFactorsI = pfsI primesI

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isqrtI :: Integer -> Integer
isqrtI = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 1 = False
isPrime k = null [(x)|x <- [2 .. isqrt k],k `mod` x == 0]

primes :: [Int]
primes =
  2 :
  3 :
  minus [5,7 ..]
        (unionAll [[p * p,p * p + 2 * p ..]|p <- tail primes])

primesI :: [Integer]
primesI =
  2 :
  3 :
  minus [5,7 ..]
        (unionAll [[p * p,p * p + 2 * p ..]|p <- tail primesI])