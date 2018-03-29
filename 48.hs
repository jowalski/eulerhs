-- Problem 48: Self powers
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
selfPowers :: Integer -> Integer
selfPowers 1 = 1
selfPowers n = n ^ n + selfPowers (n - 1)

p48 :: Integer
p48 = (selfPowers 1000) `mod` (10 ^ 10)