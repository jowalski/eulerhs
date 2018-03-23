-- Problem 21: Amicable numbers
--
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than
-- n which divide evenly into n).
--
-- If d(a) = b and d(b) = a, where a =/= b, then a and b are an amicable pair
-- and each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22,
-- 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1,
-- 2, 4, 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.
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

-- sum amicables
amicables :: [Int]
amicables =
  filter (\i -> i == (d (d i)) && (d i) /= i)
         [2 .. 10000]