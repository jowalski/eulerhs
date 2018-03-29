import Primes (primes, isPrime)

-- Problem 50: Consecutive prime sum
-- The prime 41, can be written as the sum of six consecutive primes:
--                        41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below
-- one-hundred.
-- The longest sum of consecutive primes below one-thousand that adds to a
-- prime, contains 21 terms, and is equal to 953.
-- Which prime, below one-million, can be written as the sum of the most
-- consecutive primes?
consecPrimeSums :: Int -> [(Int,Int)]
consecPrimeSums n =
  filter (isPrime . snd)
         (zip [1 ..]
              (scanl1 (+) (drop n primes)))

maxBelow :: Int -> Int -> [(Int,Int)]
maxBelow n p1 =
  if allBelow == []
     then []
     else [last allBelow]
  where allBelow =
          takeWhile ((< n) . snd)
                    (consecPrimeSums p1)

-- mostConsec :: (Int,Int)
-- mostConsec =
--   last (takeWhile ((>= firstMax) . snd)
--                   )
--   where mill = 1000
--         map head (takeWhile (/= []) (map (maxBelow mill) [1 ..]))
--
-- (fst . unzip) ((map head (takeWhile (/= []) (map (maxBelow 1000000) [1..]))))
mostConsec :: Int -> Int
mostConsec n = max
  where maxSums =
          map head
              (takeWhile (/= [])
                         (map (maxBelow n)
                              [1 ..]))
        (occs,vals) = unzip (take 100 maxSums)
        Just max = lookup (maximum occs) maxSums

p50 :: Int
p50 = mostConsec 1000000