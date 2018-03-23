-- Problem 20: Factorial digit sum
-- n! means n * (n - 1) * ... * 3 * 2 * 1
-- Find the sum of the digits in the number 100!
-- sum (map (\x -> read [x] :: Int) (show (factorial 100)))
factorial :: Integer -> Integer
factorial n =
  (scanl1 (*) ([1 .. n] :: [Integer])) !! (fromIntegral (n - 1) :: Int)