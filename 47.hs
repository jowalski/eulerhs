import Primes (primeFactors)

-- Problem 47: Distinct primes factors
-- The first two consecutive numbers to have two distinct prime factors are:
-- 14 = 2 * 7
-- 15 = 3 * 5
-- The first three consecutive numbers to have three distinct prime factors
-- are:
-- 644 = 2^2 * 7 * 23
-- 645 = 3 * 5 * 43
-- 646 = 2 * 17 * 19.
-- Find the first four consecutive integers to have four distinct primes
-- factors. What is the first of these numbers?
distinct :: Eq a
         => [a] -> Bool
distinct [] = True
distinct (x:xs) = all (/= x) xs && distinct xs

consecs :: Int -> [Int] -> [Int]
consecs n (x:xs)
  | (take (n - 1) xs) == [x + 1 .. x + n - 1] = x : (consecs n xs)
  | otherwise = consecs n xs

-- head (filter (distinct . consecFactors 2) [1..])
-- consecFactors :: Int -> Int -> [[(Int,Int)]]
-- consecFactors n num = (map primeFactors) [num .. (num + n - 1)]
-- nConsecNFactors :: Int -> [[[(Int,Int)]]]
-- nConsecNFactors n =
--   filter ((== (replicate n n)) . map length)
--          (map (consecFactors n)
--               [1 ..])
-- factorsToNum :: [(Int,Int)] -> Int
-- factorsToNum = product . map (uncurry (^))
consecFactors :: Int -> [Int]
consecFactors n =
  consecs n
          (filter ((== n) . length . primeFactors)
                  [1 ..])