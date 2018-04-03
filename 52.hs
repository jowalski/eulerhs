import Digits
import Data.List (sort)

-- Problem 52: Permuted multiples
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.
sameDigits :: [Int] -> [Int] -> Bool
sameDigits xs ys = (sort xs) == (sort ys)

allSameDigits :: [[Int]] -> Bool
allSameDigits [] = True
allSameDigits [xs] = True
allSameDigits (xs:xss) = all (sameDigits xs) xss

-- take 1
--      (filter (allSameDigits . map numToDigits . multsN 2)
--              [2 ..])
multsN :: Int -> Int -> [Int]
multsN n i = [i * m|m <- [1 .. n]]

permMults :: Int -> [Int]
permMults n =
  filter (allSameDigits . map numToDigits . multsN n)
         [2 ..]

p52 :: Int
p52 = head (permMults 6)