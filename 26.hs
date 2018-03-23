-- Problem 26: Reciprocal cycles
-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--    1/2  =  0.5
--    1/3  =  0.(3)
--    1/4  =  0.25
--    1/5  =  0.2
--    1/6  =  0.1(6)
--    1/7  =  0.(142857)
--    1/8  =  0.125
--    1/9  =  0.(1)
--   1/10  =  0.1
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can
-- be seen that ^1/[7] has a 6-digit recurring cycle.
-- Find the value of d < 1000 for which ^1/[d] contains the longest recurring
-- cycle in its decimal fraction part.
decimalrep' :: Int -> Int -> [(Int,Int)]
decimalrep' n r =
  case r of
    0 -> []
    _ ->
      (d10,r) :
      (decimalrep' n
                   (mod r10 n))
      where d10 = div r10 n
            r10 = r * 10

decimalrep :: Int -> [Int]
decimalrep n = map fst (decimalrep' n 1)

firstDup' :: Eq a
          => [a] -> [a] -> Maybe a
firstDup' ks [] = Nothing
firstDup' ks (x:xs)
  | elem x ks = Just x
  | otherwise = firstDup' (x : ks) xs

-- the first duplicate element in a list
firstDup :: Eq a
         => [a] -> Maybe a
firstDup = firstDup' []

-- note: will only get the first cycle if it starts with firstDup
firstCycle :: Eq a
           => [a] -> [a]
firstCycle xs =
  case firstDup xs of
    Nothing -> []
    Just d -> c : (takeWhile (/= d) cs)
      where c:cs = dropWhile (/= d) xs

-- map cycleLen [1..999]
-- the length of cycle in a decimal rep
cycleLen :: Int -> Int
cycleLen n = length (firstCycle (decimalrep' n 1))

-- whichMax (map cycleLen [1..999]) + 1
whichMax :: Ord a
         => [a] -> Int
whichMax xs = ans
  where max = maximum xs
        Just ans = lookup max (zip xs [0 .. length xs - 1])