-- Problem 38: Pandigital multiples
-- Take the number 192 and multiply it by each of 1, 2, and 3:
--   192 * 1 = 192
--   192 * 2 = 384
--   192 * 3 = 576
-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We
-- will call 192384576 the concatenated product of 192 and (1,2,3)
-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
-- and 5, giving the pandigital, 918273645, which is the concatenated product
-- of 9 and (1,2,3,4,5).
-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as
-- the concatenated product of an integer with (1,2, ... , n) where n > 1?
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

multiple :: Int -> Int -> [Int]
multiple start n = (concat . (map numToDigits)) ((*) <$> [start] <*> [1 .. n])

multipleTuple :: Int -> Int -> ((Int,Int),[Int])
multipleTuple s n = ((s,n),multiple s n)

-- (a -> Bool) -> [(b,a)] -> [(b,a)]
-- filterSnd p fs :: filter (p . snd) fs
p38 :: Int
p38 =
  (digitsToNum . maximum)
    (filter isPandigital (multiple <$> [0 .. 9999] <*> [1 .. 7]))