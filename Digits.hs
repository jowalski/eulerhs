module Digits where

numToDigits :: Int -> [Int]
numToDigits = reverse . digsr

digsr :: Int -> [Int]
digsr 0 = []
digsr n = (n `mod` 10) : (digsr (n `div` 10))

digitsToNum :: [Int] -> Int
digitsToNum ns = ltn ns 0
  where ltn [] val = val
        ltn ns' val =
          ltn (tail ns')
              ((val * 10) + head ns')

isPandigital :: [Int] -> Bool
isPandigital ns = length ns == 9 && all inNs [1 .. 9]
  where inNs x = elem x ns

baseN :: Int -> Int -> [Int]
baseN b = reverse . (digsrb b)

digsrb :: Int -> Int -> [Int]
digsrb _ 0 = []
digsrb b n = (n `mod` b) : (digsrb b (n `div` b))