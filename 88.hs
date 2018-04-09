import Combs (combswro)
import Data.List (nub)

-- Problem 88: Product-sum numbers
-- A natural number, N, that can be written as the sum and product of a given
-- set of at least two natural numbers, {a[1], a[2], ... , a[k]} is called a
-- product-sum number: N = a[1] + a[2] + ... + a[k] = a[1] * a[2] * ... *
-- a[k].
-- For example, 6 = 1 + 2 + 3 = 1 * 2 * 3.
-- For a given set of size, k, we shall call the smallest N with this
-- property a minimal product-sum number. The minimal product-sum numbers for
-- sets of size, k = 2, 3, 4, 5, and 6 are as follows.
-- k=2: 4 = 2 * 2 = 2 + 2
-- k=3: 6 = 1 * 2 * 3 = 1 + 2 + 3
-- k=4: 8 = 1 * 1 * 2 * 4 = 1 + 1 + 2 + 4
-- k=5: 8 = 1 * 1 * 2 * 2 * 2 = 1 + 1 + 2 + 2 + 2
-- k=6: 12 = 1 * 1 * 1 * 1 * 2 * 6 = 1 + 1 + 1 + 1 + 2 + 6
-- Hence for 2<=k<=6, the sum of all the minimal product-sum numbers is 4+6+8+12
-- = 30; note that 8 is only counted once in the sum.
-- In fact, as the complete set of minimal product-sum numbers for 2<=k<=12 is
-- {4, 6, 8, 12, 15, 16}, the sum is 61.
-- What is the sum of all the minimal product-sum numbers for 2<=k<1=2000?
isProductSum :: [Int] -> Bool
isProductSum ns = sum ns == product (tail ns)

sumGEQProd :: [Int] -> Bool
sumGEQProd ns = sum ns >= product (tail ns)

-- the maximum number of non-one values a set can have (before all products are
-- larger than sums)
maxNonOnes :: Int -> Int
maxNonOnes k =
  (length . filter (== 2) . last) $
  takeWhile sumGEQProd
            (map (onesAndTwos k)
                 [1 ..])

-- the maximum value above one to try, given the number of ones
maxN :: Int -> Int -> Int
maxN k nOnes =
  if tries == []
     then 2
     else (last . last) tries
  where tries =
          takeWhile sumGEQProd
                    (map (\x ->
                            (onesAndTwos (k - 1)
                                         (k - nOnes - 1)) ++
                            [x])
                         [3 ..])

onesAndTwos :: Int -> Int -> [Int]
onesAndTwos total twos = [total - twos] ++ (replicate twos 2)

-- onesAndTwos total twos = (replicate (total - twos) 1) ++ (replicate twos 2)
onesAndRest :: Int -> [Int] -> [Int]
onesAndRest k rest = [k - length rest] ++ rest

-- onesAndRest k rest = (replicate (k - length rest) 1) ++ rest
combsToTry :: Int -> [[Int]]
combsToTry k = concat $ map (\i -> onesAndRest k <$> addedCombs k i) is
  where is = [0 .. maxNonOnes k - 2]

addedCombs :: Int -> Int -> [[Int]]
addedCombs k i =
  combswro [2 .. (maxN k (k - m + i))]
           (m - i)
  where m = maxNonOnes k

psNums :: Int -> [[Int]]
psNums k = filter isProductSum $ combsToTry k

p88 :: Int
p88 =
  (sum . nub) $
  map (minimum . map sum . psNums)
      [2 .. 2000]

main = do putStrLn (show (p88))