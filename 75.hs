import Combs (combsworo)
import Data.List (group)
import Data.List.Ordered (sort)
import System.Environment

-- Problem 75: Singular integer right triangles
-- It turns out that 12 cm is the smallest length of wire can be bent to form
-- a right angle triangle in exactly one way, but there are many more
-- examples.
-- 12 cm: (3,4,5)
-- 24 cm: (6,8,10)
-- 30 cm: (5,12,13)
-- 36 cm: (9,12,15)
-- 40 cm: (8,15,17)
-- 48 cm: (12,16,20)
-- In contrast, some lengths of wire, like 20 cm, cannot be bent to form a
-- right angle triangle, and other lengths allow more than one solution to be
-- found; for example, using 120 cm it is possible to form exactly three
-- different right angle triangles.
-- 120 cm: (30,40,50), (20,48,52), (24,45,51)
-- Given that L is the length of the wire, for how many values of L <= 2,000,000
-- can exactly one right angle triangle be formed?
-- rightTriangle :: [Int] -> Bool
-- rightTriangle [a,b,c] = a ^ 2 + b ^ 2 == c ^ 2
-- -- rT :: Int -> [[Int]]
-- -- rT n = [| <- combsworo [1 .. n] 2,]
-- newton :: Int -> (Int,Int) -> (Int,Int)
-- newton n (x,_) = ((x + (n `div` x)) `div` 2,x)
-- -- [[a,b,c]|[a,b] <- combsworo [1 .. n] 2,Just x <- isqrt (a ^ 2 + b ^ 2)]
-- isqrt :: Int -> [Int]
-- isqrt n =
--   if n == approx ^ 2
--      then [approx]
--      else []
--   where approx =
--           (fst . head)
--             (dropWhile (\(x,x1) -> x /= x1 && x /= x1 + 1)
--                        (iterate (newton n)
--                                 (n `div` 2,0)))
-- rts :: Int -> [[Int]]
-- rts n =
--   [[a,b,c]
--   |[a,b] <- combsworo [1 .. n] 2
--   ,c <- isqrt (a ^ 2 + b ^ 2)
--   ,a + b + c < 2000000]
-- uniques :: Int -> Int
-- uniques n = (sum . filter (== 1) . map length . group . sort) (map sum (rts n))
-- m > n, m and n are coprime and not both odd
pyTriple :: [Int] -> [Int]
pyTriple [k,n,m] = [a,b,c]
  where a = k * (m ^ 2 - n ^ 2)
        b = k * (2 * m * n)
        c = k * (m ^ 2 + n ^ 2)

pyTriples :: Int -> [[Int]]
pyTriples n =
  map pyTriple
      ((:) <$> [1 .. n] <*>
       filter (\[n,m] -> isCoprime n m && (not (odd n && odd m)))
              (combsworo [1 .. n] 2))

isCoprime :: Int -> Int -> Bool
isCoprime a b = (gcd a b) == 1

p75 :: Int -> Int
p75 n =
  (sum .
   filter (== 1) . map length . group . sort . filter (<= 1500000) . map sum) (pyTriples n)

main =
  do args <- getArgs
     -- progName <- getProgName
     -- putStrLn "The arguments are:"
     putStrLn "The number of singular integer right triangles:"
     putStrLn (show (p75 (read (args !! 0))))