import Data.Char

-- Problem 24: Lexicographic permutations
-- A permutation is an ordered arrangement of objects. For example, 3124 is
-- one possible permutation of the digits 1, 2, 3 and 4. If all of the
-- permutations are listed numerically or alphabetically, we call it
-- lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
--                     012   021   102   120   201   210
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3,
-- 4, 5, 6, 7, 8 and 9?
perm :: [a] -> [[a]]
perm [] = [[]]
perm [x] = [[x]]
perm xs = [(xs !! i) : ps|i <- [0 .. l],ps <- perm (takeNth i xs)]
  where l = length xs - 1
        takeNth n xs = (take n xs) ++ (drop (n + 1) xs)

-- nthPerm 1000000 [0..9]
nthPerm :: Int -> [Int] -> String
nthPerm n ns = map intToDigit ((perm ns) !! (n - 1))