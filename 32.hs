-- Problem 32: Pandigital products
-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once; for example, the 5-digit number, 15234,
-- is 1 through 5 pandigital.
-- The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.
-- Find the sum of all products whose multiplicand/multiplier/product
-- identity can be written as a 1 through 9 pandigital.
-- HINT: Some products can be obtained in more than one way so be sure to
-- only include it once in your sum.
takeNth :: [a] -> Int -> (a,[a])
takeNth xs n = (head bs,as ++ (tail bs))
  where (as,bs) = splitAt n xs

allsplits :: [a] -> [(a,[a])]
allsplits xs =
  map (takeNth xs)
      [0 .. length xs - 1]

combswr :: [a] -> Int -> [[a]]
combswr [] _ = [[]]
combswr _ 0 = [[]]
combswr [x] _ = [[x]]
combswr xs n = [a : cs|(a,bs) <- allsplits xs,cs <- combswr bs (n - 1)]

-- 99*99 < 10000, so equals sign must be at least be after pos. 4
-- 111*111 > 9999, so must be before pos. 5, so only one spot
-- panprod :: [Int] -> Int -> Bool
-- panprod ns mpos =
digitsToNum :: [Int] -> Int
digitsToNum ns = ltn ns 0
  where ltn [] val = val
        ltn ns' val =
          ltn (tail ns')
              ((val * 10) + head ns')

numToDigits :: Int -> [Int]
numToDigits = reverse . digsr

digsr :: Int -> [Int]
digsr 0 = []
digsr n = (n `mod` 10) : (digsr (n `div` 10))

-- Note: assumes as ++ bs are unique, length (as ++ bs) == 5
-- isPandig xs ys = length nums == 9 && (all (\i -> elem i nums)) [1 .. 9]
--   where nums = xs ++ ys
isPandigital :: ([Int],[Int]) -> [Int] -> Bool
isPandigital (as,bs) ys =
  length (foldl (\xs x ->
                   if (elem x xs)
                      then []
                      else (x : xs))
                (0 : (as ++ bs))
                ys) ==
  10

panprods :: [([Int],[Int])]
panprods = filter panCheck (splitAt <$> [1 .. 3] <*> combswr [1 .. 9] 5)

panCheck :: ([Int],[Int]) -> Bool
panCheck x =
  isPandigital x
               ((numToDigits . lprod) x)

lprod :: ([Int],[Int]) -> Int
lprod (xs,ys) = digitsToNum xs * digitsToNum ys

insertOrdUnique :: Ord a
                => a -> [a] -> [a]
insertOrdUnique i [] = [i]
insertOrdUnique i (x:xs)
  | i < x = i : x : xs
  | i == x = x : xs
  | otherwise = x : (insertOrdUnique i xs)

makeOrdSet :: Ord a
           => [a] -> [a]
makeOrdSet = foldr insertOrdUnique []

p32 :: Int
p32 = (sum . makeOrdSet . (map lprod)) panprods