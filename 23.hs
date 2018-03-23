import System.IO

-- ghc 23.hs -Odph -o 23
-
-- Problem 23: Non-abundant sums
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors
-- of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
-- number.
-- A number whose proper divisors are less than the number is called
-- deficient and a number whose proper divisors exceed the number is called
-- abundant.
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant numbers is
-- 24. By mathematical analysis, it can be shown that all integers greater
-- than 28123 can be written as the sum of two abundant numbers. However,
-- this upper limit cannot be reduced any further by analysis even though it
-- is known that the greatest number that cannot be expressed as the sum of
-- two abundant numbers is less than this limit.
-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.
divisors :: Int -> [Int]
divisors n =
  xs ++
  if last xs == last ys
     then tail (reverse ys)
     else (reverse ys)
  where (xs,ys) = unzip [(d,n `div` d)|d <- [1 .. sroot],n `mod` d == 0]
        sroot = (floor . sqrt . fromIntegral) n

d :: Int -> Int
d = (sum . tail . reverse . divisors)

abundants :: [Int]
abundants =
  filter (\x -> d x > x)
         [1 ..]

isSumAbun :: Int -> Bool
isSumAbun n = any (\x -> elem (n - x) abuns) abuns
  where abuns = takeWhile (< n) abundants

listNotSumAbuns :: [Int]
listNotSumAbuns = [(n)|n <- [1 .. 28123],(not . isSumAbun) n]

listSumAbuns :: [Int]
listSumAbuns = [(n)|n <- [1 .. 28123],isSumAbun n]

main :: IO ()
main = (putStr . show . sum) listNotSumAbuns
