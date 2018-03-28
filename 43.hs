import Digits (numToDigits, digitsToNum)
import Primes (primes)
import Combs (combswr)

-- Problem 43: Sub-string divisibility
-- The number, 1406357289, is a 0 to 9 pandigital number because it is made
-- up of each of the digits 0 to 9 in some order, but it also has a rather
-- interesting sub-string divisibility property.
-- Let d[1] be the 1st digit, d[2] be the 2nd digit, and so on. In this
-- way, we note the following:
--   * d[2]d[3]d[4]=406 is divisible by 2
--   * d[3]d[4]d[5]=063 is divisible by 3
--   * d[4]d[5]d[6]=635 is divisible by 5
--   * d[5]d[6]d[7]=357 is divisible by 7
--   * d[6]d[7]d[8]=572 is divisible by 11
--   * d[7]d[8]d[9]=728 is divisible by 13
--   * d[8]d[9]d[10]=289 is divisible by 17
-- Find the sum of all 0 to 9 pandigital numbers with this property.
-- isPandigital :: [Int] -> Bool
-- isPandigital ns = all inNs [1 .. length ns]
--   where inNs x = elem x ns
pandigitals :: Int -> [[Int]]
pandigitals n = combswr (reverse (take n ns)) n
  where ns =
          (if n == 10
              then [0]
              else []) ++
          [1 .. 9]

get3At :: Int -> [Int] -> [Int]
get3At i = take 3 . drop i

digsDivisible :: Int -> [Int] -> Bool
digsDivisible n = (== 0) . (`mod` n) . digitsToNum

isSubstrDiv :: [Int] -> Bool
isSubstrDiv ns =
  all (uncurry digsDivisible)
      (zip primes (tail (get3At <$> [0 .. 7] <*> [ns])))

p43 :: Int
p43 = sum (map digitsToNum (filter isSubstrDiv (pandigitals 10)))