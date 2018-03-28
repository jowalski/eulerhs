import Primes (isqrt)
import Data.List (group)
import Data.List.Ordered (sort)

-- Problem 39: Integer right triangles
-- If p is the perimeter of a right angle triangle with integral length
-- sides, {a,b,c}, there are exactly three solutions for p = 120.
--                     {20,48,52}, {24,45,51}, {30,40,50}
-- For which value of p < 1000, is the number of solutions maximised?
hypot :: Int -> Int -> [[Int]]
hypot a b =
  if (c ^ 2 == ab2)
     then [[p,a,b,c]]
     else []
  where ab2 = a ^ 2 + b ^ 2
        c = isqrt ab2
        p = a + b + c

triangles :: Int -> [[Int]]
triangles pmax =
  [(h)|b <- [1 .. pmax],a <- [1 .. b],h <- hypot a b,head h < 1000]

p39 :: Int
p39 = (head . head) (filter ((== lmax) . length) ps)
  where ps = (group . sort . map head) (triangles 999)
        lmax = maximum (map length ps)