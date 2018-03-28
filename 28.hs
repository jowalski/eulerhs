-- Problem 28: Number spiral diagonals
-- Starting with the number 1 and moving to the right in a clockwise
-- direction a 5 by 5 spiral is formed as follows:
--                               21 22 23 24 25
--                               20  7  8  9 10
--                               19  6  1  2 11
--                               18  5  4  3 12
--                               17 16 15 14 13
-- It can be verified that the sum of both diagonals is 101.
-- What is the sum of both diagonals in a 1001 by 1001 spiral formed in the
-- same way?
--
-- 13 14 15 16
-- 12  3  4  5
-- 11  2  1  6
-- 10  9  8  7
--
-- 31 32 33 34 35 36
-- 30 13 14 15 16 17
-- 29 12  3  4  5 18
-- 28 11  2  1  6 19
-- 27 10  9  8  7 20
-- 26 25 24 23 22 21
--
-- top-left, bottom-right elements
tlbr :: Int -> [Int]
tlbr n = [n ^ 2 - n + 1,n ^ 2 - (3 * n) + 3]

-- top-right, bottom-left elements
trbl :: Int -> [Int]
trbl n = [n ^ 2,(n - 1) ^ 2 + 1]

diag :: (Int -> [Int]) -> Int -> [Int]
diag _ 0 = []
diag _ 1 = [1]
diag d n = d n ++ diag d (n - 2)

sumDiag :: Int -> Int
sumDiag n =
  sum (diag tlbr n) + sum (diag trbl n) -
  if (odd n)
     then 1
     else 0