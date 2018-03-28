-- Problem 36: Double-base palindromes
-- The decimal number, 585 = 1001001001[2] (binary), is palindromic in both
-- bases.
-- Find the sum of all numbers, less than one million, which are palindromic
-- in base 10 and base 2.
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)
baseN :: Int -> Int -> [Int]
baseN b = reverse . (digsrb b)

digsrb :: Int -> Int -> [Int]
digsrb _ 0 = []
digsrb b n = (n `mod` b) : (digsrb b (n `div` b))

isBasePalindrome :: Int -> Int -> Bool
isBasePalindrome b n = db == reverse db
  where db = digsrb b n

isDoubleBase :: Int -> Bool
isDoubleBase n = isBasePalindrome 2 n && isBasePalindrome 10 n

p36 :: Int
p36 = sum (filter isDoubleBase [1 .. 999999])