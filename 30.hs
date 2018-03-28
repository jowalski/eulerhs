import Data.List (sort)
import Data.Char (digitToInt)

-- Problem 30: Digit fifth powers
-- Surprisingly there are only three numbers that can be written as the sum
-- of fourth powers of their digits:
--   1634 = 1^4 + 6^4 + 3^4 + 4^4
--   8208 = 8^4 + 2^4 + 0^4 + 8^4
--   9474 = 9^4 + 4^4 + 7^4 + 4^4
-- As 1 = 1^4 is not a sum it is not included.
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.
-- all same => 10
-- 2 (1,4 or 2,3)=>10*9 * 2
-- 3 (1,1,3 or 1,2,2) => 10*9*7 *2
-- 4 (1,1,1,2) => 10*9*8*7
-- 5 (1,1,1,1,1) => 10*9*8*7*6/5*4*3*2*1
combsInOrder :: Ord a
             => [a] -> Int -> [[a]]
combsInOrder [] _ = [[]]
combsInOrder _ 0 = [[]]
combsInOrder xs n =
  [c
  |ds <-
     map (\i -> drop i objs)
         [0 .. length objs - 1]
  ,c <-
     map ([head ds] ++)
         (combsInOrder ds
                       (n - 1))]
  where objs = sort xs

powSums :: Int -> [([Int],Int)]
powSums n = map (\c -> (c,sum (map getfp c))) combs
  where fps = [i ^ n|i <- [0 .. 9]]
        getfp = (!!) fps
        combs =
          combsInOrder [0 .. 9]
                       (n + 1)

-- convert a number to a list of digits
digits :: Int -> Int -> [Int]
digits 1 n = [n]
digits i n =
  ith :
  (digits (i - 1)
          (n - (ith * i10)))
  where i10 = 10 ^ (i - 1)
        ith = n `div` i10

-- is the list of digits in the number (irrespective of order)?
sumEqDigits :: ([Int],Int) -> Bool
sumEqDigits (ns,s) = ns == sort (digits (length ns) s)

-- filter (\(ns,s) -> ns == sort (digits 4 s)) (powSums 4)
-- filter ((>9999) . snd) digFifthPows
-- sum (map snd digFifthPows)
prob30 :: Int
prob30 = (sum . (filter (> 1)) . (map snd) . (filter sumEqDigits)) (powSums 5)