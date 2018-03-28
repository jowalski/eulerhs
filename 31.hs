-- Problem 31: Coin sums
-- In England the currency is made up of pound, -L-, and pence, p, and there
-- are eight coins in general circulation:
--   1p, 2p, 5p, 10p, 20p, 50p, -L-1 (100p) and -L-2 (200p).
-- It is possible to make -L-2 in the following way:
--   1 * -L-1 + 1 * 50p + 2 * 20p + 1 * 5p + 1 * 2p + 3 * 1p
-- How many different ways can -L-2 be made using any number of coins?
reachTotal :: [Int] -> Int -> Int
reachTotal vals t
  | t < 0 = 0
  | vals == [] = 0
  | t == 0 = 1
  | otherwise =
    (reachTotal (tail vals)
                t) +
    (reachTotal vals
                (t - head vals))

p31 :: Int
p31 =
  reachTotal [200,100,50,20,10,5,2,1]
             200