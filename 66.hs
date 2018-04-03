import Primes (isqrt, isPrime, primeFactorsI, isqrtI)
import Data.List.Ordered (minus, isect)

-- Problem 66: Diophantine equation
-- Consider quadratic Diophantine equations of the form:
--                               x^2 - Dy^2 = 1
-- For example, when D=13, the minimal solution in x is 649^2 - 13 * 180^2 =
-- 1.
-- It can be assumed that there are no solutions in positive integers when D
-- is square.
-- By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
-- following:
-- 3^2 - 2 * 2^2 = 1
-- 2^2 - 3 * 1^2 = 1
-- 9^2 - 5 * 4^2 = 1
-- 5^2 - 6 * 2^2 = 1
-- 8^2 - 7 * 3^2 = 1
-- Hence, by considering minimal solutions in x for D <= 7, the largest x is
-- obtained when D=5.
-- Find the value of D <= 1000 in minimal solutions of x for which the largest
-- value of x is obtained.
--
-- JPK Note: see wikipedia on Pell's equation and the solution using the
-- convergents of the continued fraction of square roots of D. That is very
-- quick, other methods take too long.
dioph :: Integer -> (Integer,Integer) -> Integer
dioph d (x,y) = x ^ 2 - (d * y ^ 2)

squares :: [Integer]
squares = [i ^ 2|i <- [1 ..]]

nonsquares :: [Integer]
nonsquares = [1 ..] `minus` squares

-- -- Note: (x + 1)(x-1) = D * y ^2
-- -- this is (x+1)(x-1)
-- xp1xm1 :: Integer -> Integer
-- xp1xm1 x = (x + 1) * (x - 1)
-- -- this is D*y^2
-- dy2 :: Integer -> Integer -> Integer
-- dy2 d y = d * y ^ 2
-- -- we can filter out any terms not divisible by D
-- xpms :: Integer -> [Integer]
-- xpms divisor =
--   filter ((== 0) . (`mod` divisor))
--          (map xp1xm1 [1 ..])
-- dy2s :: Integer -> [Integer]
-- dy2s d = [dy2 d y|y <- [1 ..]]
-- dy2sX :: Integer -> [Integer]
-- dy2sX d = [dy2 d y|y <- [1 ..]]
-- minIsect :: Integer -> Integer
-- minIsect d =
--   head (isect (xpmults d)
--               (dy2s d))
-- -- minX :: Integer -> Integer
-- -- minX d =
-- --   case lookup (minIsect d)
-- --               (zip (xpms 1)
-- --                    [1 ..]) of
-- --     Nothing -> 0
-- --     Just x -> x
-- -- minXs ((filter (/= 109) . filter (/= 106) . filter (/= 97) . filter (/= 61)) nonsquares)
-- -- minXs :: [Integer] -> [(Integer,Integer)]
-- -- minXs ns = (zip ns . map minX) ns
-- xpmults :: Integer -> [Integer]
-- xpmults n = concat [[(i - 2) * i,i * (i + 2)]|i <- [n,n * 2 ..]]
contFrac :: Int -> Int -> [Int]
contFrac m n = [(a_i)|(_,_,a_i) <- iterate cf (m_0,d_0,a_0)]
  where a_0 = m `div` n
        m_0 = m `mod` n
        d_0 = n
        cf (m_i,d_i,a_i) = (m_j,d_j,a_j)
          where d_j = m_i       -- 1,0
                m_j = d_i `mod` m_i -- 0
                a_j = d_i `div` m_i -- 2

sqContFrac :: Integer -> [Integer]
sqContFrac n = [(a_i)|(_,_,a_i) <- iterate scf (m_0,d_0,a_0)]
  where a_0 = (truncate . sqrt . fromIntegral) n
        m_0 = 0
        d_0 = 1
        scf (m_i,d_i,a_i) = (m_j,d_j,a_j)
          where m_j = d_i * a_i - m_i -- a_0
                d_j = (n - m_j * m_j) `div` d_i -- n-n^2
                a_j = (a_0 + m_j) `div` d_j

sqConvergents :: Integer -> [(Integer,Integer)]
sqConvergents n =
  [(h_i,k_i)|(h_i,k_i,_,_,i) <- (tail . iterate convs) (hm1,km1,hm2,km2,0)]
  where scf = ((sqContFrac n) !!)
        hm2 = 0
        km2 = 1
        hm1 = 1
        km1 = 0
        convs (hi1,ki1,hi2,ki2,i) = (hi,ki,hi1,ki1,i + 1)
          where hi = (scf i) * hi1 + hi2
                ki = (scf i) * ki1 + ki2

minX :: Integer -> Integer
minX n = (fst . head . filter ((== 1) . dioph n)) (sqConvergents n)

minXs :: [Integer] -> [(Integer,Integer)]
minXs ns = (zip ns . map minX) ns

p66 :: Integer
p66 =
  case lookup (maximum xs)
              (zip xs ns) of
    Nothing -> -1
    Just x -> x
  where solns =
          takeWhile ((<= 1000) . fst)
                    (minXs nonsquares)
        (ns,xs) = unzip solns