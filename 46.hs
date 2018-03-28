import Primes (primes, isPrime, isqrt)
import Data.List.Ordered (nub, sort)

-- Problem 46: Goldbach's other conjecture
-- It was proposed by Christian Goldbach that every odd composite number can
-- be written as the sum of a prime and twice a square.
-- 9 = 7 + 2 * 1^2
-- 15 = 7 + 2 * 2^2
-- 21 = 3 + 2 * 3^2
-- 25 = 7 + 2 * 3^2
-- 27 = 19 + 2 * 2^2
-- 33 = 31 + 2 * 1^2
-- It turns out that the conjecture was false.
-- What is the smallest odd composite that cannot be written as the sum of a
-- prime and twice a square?
oddComps :: [Int]
oddComps =
  filter (not . isPrime)
         [3,5 ..]

sps :: Int -> Int -> Int
sps p s = p + 2 * s ^ 2

isPrimeSquareSum :: Int -> Bool
isPrimeSquareSum n = guessSps n /= []

guessSps :: Int -> [Int]
guessSps n =
  dropWhile (/= n)
            [sps p s|p <- takeWhile (< n) primes,s <- [1 .. isqrt (n `div` 2)]]

sumPrimeSquares :: Int -> [Int]
sumPrimeSquares pn =
  filter (not . isPrime)
         [sps (primes !! pn) s|s <- [1 ..]]

-- p46 :: Int
-- p46 = head (filter (not . isPrimeSquareSum) oddComps)
nthPrime :: Int -> Int
nthPrime n = primes !! n

-- all "sumprimesquare"s up to the nth prime number
spsUpTo :: Int -> [Int]
spsUpTo n =
  (nub . sort . concat)
    (map (takeWhile (< (nthPrime n)))
         (map sumPrimeSquares [1 .. n]))

firstDiff :: [Int] -> [Int] -> [Int]
firstDiff [] [] = []
firstDiff [] (y:ys) = [y]
firstDiff (x:xs) [] = [x]
firstDiff (x:xs) (y:ys)
  | x == y = firstDiff xs ys
  | x < y = [x]
  | otherwise = [y]

missingComps :: Int -> [Int]
missingComps n =
  firstDiff (takeWhile (< (nthPrime n)) oddComps)
            (spsUpTo n)

p46 :: Int
p46 = head (concat (map missingComps [1000 ..]))