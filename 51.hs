import Primes
import Combs
import Digits
import Data.List

-- Problem 51: Prime digit replacements
-- By replacing the 1st digit of *57, it turns out that six of the possible
-- values: 157, 257, 457, 557, 757, and 857, are all prime.
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this
-- 5-digit number is the first example having seven primes, yielding the
-- family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently
-- 56003, being the first member of this family, is the smallest prime with
-- this property.
-- Find the smallest prime which, by replacing part of the number (not
-- necessarily adjacent digits) with the same digit, is part of an eight
-- prime value family.
--
-- approach #1: generate primes starting from x (56003), create all possible
-- replacements, count # of primes
--
-- approach #2: generate all combs of N digits (with m misssing), generate
-- possible replacements, count # of primes
opMap
  :: (a -> a -> b) -> (c -> a) -> (c -> c -> b)
opMap op f =
  (\x y ->
     op (f x)
        (f y))

-- note: it's assumed ns are in order
-- replace :: [a] -> Int -> [a] -> [[a]]
-- replace rs n ds = map (\r -> as ++ (r : bs)) rs
--   where (as,_:bs) = splitAt n ds
replace :: [a] -> [Int] -> [a] -> [[a]]
replace rs [] ds = [ds]
replace rs (n:ns) ds =
  (++) <$> (map (\r -> as ++ [r]) rs) <*>
  (replace rs (map (+ (-(n + 1))) ns) bs)
  where (as,_:bs) = splitAt n ds

replaceAll :: [a] -> [Int] -> [a] -> [[a]]
replaceAll rs [] ds = [ds]
replaceAll rs (n:ns) ds =
  concat (map (\r ->
                 (++) <$> [as ++ [r]] <*>
                 (replace [r]
                          (map (+ (-(n + 1))) ns)
                          bs))
              rs)
  where (as,_:bs) = splitAt n ds

-- replaceDigits (0:ns) (d:ds) =
--   (:) <$> [1 .. 9] <*>
--   (replaceDigits (map pred ns)
--                  ds)
replaceDigits :: [Int] -> [Int] -> [[Int]]
replaceDigits ns ds =
  replaceAll
    [if head ns == 0
        then 1
        else 0 .. 9]
    ns
    ds

-- replaceDigits (0:ns) ds =
--   replaceAll [1 .. 9]
--              (0 : ns)
--              ds
-- replaceDigits ns ds =
--   replaceAll [0 .. 9]
--              ns
--              ds
replacePositions :: [Int] -> [[Int]]
replacePositions ns =
  (filter (sameAtPos ns) . concat . map (combsworo [0 .. l])) [1 .. l]
  where l = length ns - 1

sameAtPos :: Eq a
          => [a] -> [Int] -> Bool
sameAtPos xs ns = all (== head es) (tail es)
  where es = map (xs !!) ns

replacementNums :: [Int] -> [[[Int]]]
replacementNums ds = replaceDigits <$> replacePositions ds <*> [ds]

-- replacementPrimes :: [Int] -> [[[Int]]]
-- replacementPrimes ds =
--   [(replace [1 .. 9]
--             [0]
--             ds)] ++
--   (replace [0 .. 9] <$> [[1 .. length ds - 1]] <*> [ds])
-- replacementPrimes ds = replace [0 .. 9] <$> [0 .. length ds - 1] <*> [ds]
numRepPrimes :: [Int] -> [Int]
numRepPrimes ds =
  map (length . filter (isPrime . digitsToNum))
      (replacementNums ds)

repPrimes :: Int -> [[Int]]
repPrimes n =
  map (filter (isPrime) . map digitsToNum)
      ((replacementNums . numToDigits) n)

p51 :: Int
p51 =
  head (filter (any (> 7) . numRepPrimes . numToDigits)
               (dropWhile (< 56003) primes))