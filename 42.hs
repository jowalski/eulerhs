import Read (readCommaQuotes)
import Data.Char (ord)

-- Problem 42: Coded triangle numbers
-- The n-th term of the sequence of triangle numbers is given by, t[n] =
-- (1/2)n(n+1); so the first ten triangle numbers are:
--                  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t[10]. If the word
-- value is a triangle number then we shall call the word a triangle word.
-- Using words.txt, a 16K text file containing nearly two-thousand common
-- English words, how many are triangle words?
-- This problem references the following resources:
-- words.txt
t :: Int -> Int
t n = n * (n + 1) `div` 2

-- only for uppercase
charNum :: Char -> Int
charNum c = ord c - baseChar + 1

wordSum :: String -> Int
wordSum = sum . (map charNum)

baseChar = ord 'A'

triangles :: [Int]
triangles = map t [1 ..]

commonWords :: IO [String]
commonWords = readCommaQuotes "words.txt"

codedNums :: [String] -> [String]
codedNums wds = (filter (\x -> elem (wordSum x) trianglesCache)) wds
  where trianglesCache = takeWhile (< maxWordSum) triangles
        maxWordSum = (maximum . map wordSum) wds

p42 :: IO Int
p42 = commonWords >>= return . length . codedNums