import System.IO

main :: IO ()
main = (putStr . show) pyTriplet

-- Problem 9: Special Pythagorean triplet
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for
-- which,
--                              a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
c :: Int -> Int -> Int
c a b = (1000 - a - b)

pyTriplet :: Int
pyTriplet =
  (product . head)
    [[a,b,c a b]|a <- [1 .. 1000],b <- [1 .. 1000],a ^ 2 + b ^ 2 == (c a b) ^ 2]