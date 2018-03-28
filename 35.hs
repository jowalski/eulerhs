import Data.List.Ordered (minus, union, unionAll)

-- Problem 35: Circular primes
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.
-- How many circular primes are there below one million?
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime k = null [(x)|x <- [2 .. isqrt k],k `mod` x == 0]

primes :: [Int]
primes =
  2 :
  3 :
  minus [5,7 ..]
        (unionAll [[p * p,p * p + 2 * p ..]|p <- tail primes])

-- takeNth :: [a] -> Int -> (a,[a])
-- takeNth xs n = (head bs,as ++ (tail bs))
--   where (as,bs) = splitAt n xs
-- allsplits :: [a] -> [(a,[a])]
-- allsplits xs =
--   map (takeNth xs)
--       [0 .. length xs - 1]
-- combswr :: [a] -> Int -> [[a]]
-- combswr [] _ = [[]]
-- combswr _ 0 = [[]]
-- combswr [x] _ = [[x]]
-- combswr xs n = [a : cs|(a,bs) <- allsplits xs,cs <- combswr bs (n - 1)]
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

rotateNums :: [Int] -> [[Int]]
rotateNums ns = map nthcycle [0 .. len - 1]
  where nthcycle n = (take len (drop n cyc))
        len = length ns
        cyc = cycle ns

-- rotateNums :: [Int] -> [[Int]]
-- rotateNums ns = rns (length ns) ns
--   where rns 0 _ = []
--         rns l (m:ms) =
--           (m : ms) :
--           (rns (ms ++ [m])
--                (l - 1))
isCircprime :: Int -> Bool
isCircprime n =
  all (isPrime . digitsToNum)
      (rotateNums ds)
  where ds = numToDigits n

p35 :: Int
p35 = length (filter isCircprime (takeWhile (< 1000000) primes))