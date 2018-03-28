import Data.List (sort)

-- Problem 34: Digit factorials
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

factscache = map factorial [0 .. 9]

fact :: Int -> Int
fact n = factscache !! n

factSum :: [Int] -> Int
factSum = sum . (map fact)

numToDigits :: Int -> [Int]
numToDigits = reverse . digsr

digsr :: Int -> [Int]
digsr 0 = []
digsr n = (n `mod` 10) : (digsr (n `div` 10))

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

-- assumes ns is already sorted
isDigitFactorial :: [Int] -> Bool
isDigitFactorial ns = sort (numToDigits (factSum ns)) == ns

digitFactorials :: [[Int]]
digitFactorials =
  ((filter isDigitFactorial) . concat) (combsInOrder [0 .. 9] <$> [2 .. 8])

p34 :: Int
p34 = (sum . (map factSum)) digitFactorials