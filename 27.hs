import Data.List.Ordered (minus, union, unionAll)

--
-- Problem 27: Quadratic primes
-- Euler published the remarkable quadratic formula:
--                                n^2 + n + 41
-- It turns out that the formula will produce 40 primes for the consecutive
-- values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41
-- is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly
-- divisible by 41.
-- Using computers, the incredible formula  n^2 - 79n + 1601 was discovered,
-- which produces 80 primes for the consecutive values n = 0 to 79. The
-- product of the coefficients, 79 and 1601, is 126479.
-- Considering quadratics of the form:
--   n^2 + an + b, where |a| < 1000 and |b| < 1000
--                               where |n| is the modulus/absolute value of n
--                                                e.g. |11| = 11 and |-4| = 4
-- Find the product of the coefficients, a and b, for the quadratic
-- expression that produces the maximum number of primes for consecutive
-- values of n, starting with n = 0.
quad :: Int -> Int -> Int -> Int
quad a b n = n ^ 2 + (a * n) + b

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime k = null [(x)|x <- [2 .. isqrt k],k `mod` x == 0]

quadPrimes :: Int -> Int -> [Int]
quadPrimes a b =
  (takeWhile (\n -> isPrime n && (n > 0))
             (map (quad a b)
                  [0 ..]))

numQuadPrimes :: Int -> Int -> Int
numQuadPrimes a b =
  length (takeWhile (\n -> isPrime n && (n > 0))
                    (map (quad a b)
                         [0 ..]))

tryQuadPrimes :: [(Int,(Int,Int))]
tryQuadPrimes =
  [(numQuadPrimes a b,(a,b))|a <- [-999 .. 999],b <- [-999 .. 999]]

maxQuadPrimes :: Int
maxQuadPrimes =
  maximum [(numQuadPrimes a b)|a <- [-999 .. 999],b <- [-999 .. 999]]

-- mostQuadPrimes
mostQuadPrimes :: Maybe (Int,Int)
mostQuadPrimes = lookup maxQuadPrimes tryQuadPrimes