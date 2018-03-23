import Data.Char

-- Problem 16: Power digit sum
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- What is the sum of the digits of the number 2^1000?
-- sumPowDigits 1000
sumPowDigits :: Int -> Int
sumPowDigits n = (sum . (map (\x -> read [x])) . show) (2 ^ n)