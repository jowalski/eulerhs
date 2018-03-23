-- Problem 6: Sum square difference
-- The sum of the squares of the first ten natural numbers is,
--                        1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
--                     (1 + 2 + ... + 10)^2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the first ten
-- natural numbers and the square of the sum is 3025 - 385 = 2640.
-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.
sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares xs = sum [x ^ 2|x <- xs]

squareSum :: [Int] -> Int
squareSum [] = 0
squareSum xs = ((^ 2) . sum) xs

ssDiff :: [Int] -> Int
ssDiff xs = squareSum xs - sumSquares xs