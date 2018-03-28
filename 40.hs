import Digits (numToDigits)

-- Problem 40: Champernowne's constant
-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--                   0.123456789101112131415161718192021...
--                                ^
-- It can be seen that the 12th digit of the fractional part is 1.
-- If d[n] represents the n-th digit of the fractional part, find the value
-- of the following expression.
--     d[1] * d[10] * d[100] * d[1000] * d[10000] * d[100000] * d[1000000]
champernownes :: [Int]
champernownes = (concat . map numToDigits) [1 ..]

p40 :: Int
p40 =
  product ((!!) <$> [champernownes] <*>
           (map (+ (-1))
                ((^) <$> [10] <*> [0 .. 6])))