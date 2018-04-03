import Digits

-- Problem 53: Combinatoric selections
-- There are exactly ten ways of selecting three from five, 12345:
--            123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
-- In combinatorics, we use the notation, nCr(5,3) = 10.
-- In general,
-- nCr(n,r) = n!/(r!(n-r)!), where r =< n, n! = n * (n1) * ... * 3 * 2 * 1,
-- and 0! = 1.
-- It is not until n = 23, that a value exceeds one-million: nCr(23,10) =
-- 1144066.
-- How many values of nCr(n,r), for 1 =< n =< 100, are greater than one-million?
-- ncr :: Int -> Int -> Int
-- ncr n r =
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

factList :: Integer -> Integer -> [Integer]
factList 0 _ = []
factList 1 _ = []
factList n m = reverse [m + 1 .. n]

combList
  :: Integer -> Integer -> ([Integer],[Integer])
combList n r = (factList n maxnr,factList minnr 0)
  where maxnr = maximum [r,n - r]
        minnr = minimum [r,n - r]

evalCL :: ([Integer],[Integer]) -> Integer
evalCL (ns,ds) = product ns `div` product ds

combPairs :: Integer -> [(Integer,Integer)]
combPairs n' = [(n,r)|n <- [1 .. n'],r <- [0 .. n]]

p53 :: Int
p53 =
  length (filter ((> 1000000) . evalCL . uncurry combList)
                 (combPairs 100))