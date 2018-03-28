import Digits (numToDigits, digitsToNum)
import Primes (isPrime)
import Combs (combswr)

-- Problem 41: Pandigital prime
-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
-- What is the largest n-digit pandigital prime that exists?
isPandigital :: [Int] -> Bool
isPandigital ns = all inNs [1 .. length ns]
  where inNs x = elem x ns

pandigitals :: Int -> [[Int]]
pandigitals n = combswr (reverse (take n [1 .. 9])) n

allPanprimes :: [[Int]]
allPanprimes =
  filter (isPrime . digitsToNum)
         (concat (pandigitals <$> reverse [1 .. 9]))

p41 :: Int
p41 = (digitsToNum . head) allPanprimes