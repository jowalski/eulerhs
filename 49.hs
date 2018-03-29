import Combs (combsworo, combswor, combswr)
import Digits (numToDigits, digitsToNum)
import Primes (isPrime)
import Data.List.Ordered (sort, nub)
import Data.List (group)

-- Problem 49: Prime permutations
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one
-- another.
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
-- primes, exhibiting this property, but there is one other 4-digit
-- increasing sequence.
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?
dig4s :: [[Int]]
dig4s = combsworo [0 .. 9] 4

primePermutations :: [Int] -> [Int]
primePermutations ns =
  (filter isPrime . filter (> 999) . map digitsToNum) (combswr ns 4)

-- all permutations of 4 digits of which at least 3 are prime
primePerm3s :: [[Int]]
primePerm3s =
  filter ((>= 3) . length)
         (map primePermutations dig4s)

-- get all differences between n primes of group (already sorted in order)
permDiffs :: Int -> [Int] -> [[Int]]
permDiffs n ns =
  map (diffs . (map (ns !!)))
      (permDiffIdxs n ns)

permDiffIdxs :: Int -> [Int] -> [[Int]]
permDiffIdxs n ns = combsworo [0 .. length ns - 1] n

diffs :: [Int] -> [Int]
diffs ys =
  tail (foldl (\xs x -> x : (x - head xs) : (tail xs))
              [head ys]
              (tail ys))

-- filter (allSame)
--        (map (permDiffs 3)
--             (take 3 primePerm3s))
allSame :: Eq a
        => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

which :: [Bool] -> [Int]
which bs = which' 0 bs
  where which' _ [] = []
        which' n (b':bs') =
          (if b'
              then (n :)
              else (id)) (which' (n + 1) bs')

selectIdxs :: [a] -> [Int] -> [a]
selectIdxs xs is = map (xs !!) is

-- filter (== 3330)
primePermProperty :: [[Int]]
primePermProperty =
  filter (any id . map (all (== 3330)) . permDiffs 3) primePerm3s

ppps :: [String]
ppps = (nub . showVals) (zipWith selectIdxs ppp (map head whichSeqs))
  where ppp = primePermProperty
        sameDiffs = map (permDiffs 3) ppp
        sameIdxs = map (permDiffIdxs 3) ppp
        whichDiffs = map (which . map (all (== 3330))) sameDiffs
        whichSeqs = zipWith selectIdxs sameIdxs whichDiffs
        showVals = map (concat . map show . concat . map numToDigits)