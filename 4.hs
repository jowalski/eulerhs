-- Problem 4: Largest palindrome product
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 * 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
--
-- (99,99) (98,99) (97,99) (96,99)
--         (98,98) (97,98) (96,98)
--                 (97,97) (96,97)
--
type MaybeProd = (Maybe Int,Int,Int)

calcProd :: MaybeProd -> MaybeProd
calcProd (Nothing,x,y) = (Just (x * y),x,y)
calcProd z = z

-- merge two ordered lists of Int pairs (ordered by the product of #s in the
-- pair)
mergePairs
  :: [MaybeProd] -> [MaybeProd] -> [MaybeProd]
mergePairs [] xs = map calcProd xs
mergePairs xs [] = map calcProd xs
mergePairs (x:xs) (y:ys)
  | px > py =
    px :
    (mergePairs xs
                (y : ys))
  | otherwise =
    py :
    (mergePairs (x : xs)
                ys)
  where px = calcProd x
        py = calcProd y

-- create a list of ordered lists of Int pairs
pairLists :: Int -> [[MaybeProd]]
pairLists n = [[(Nothing,x,n')|x <- reverse [1 .. n']]|n' <- reverse [1 .. n]]

-- create a list of Int pairs, ordered by their product
orderedPairs :: Int -> [MaybeProd]
orderedPairs n = foldl mergePairs [] (pairLists n)

palindromic :: Maybe Int -> Bool
palindromic Nothing = False
palindromic (Just n) = s == reverse s
  where s = show n

-- firstPalindromic 999
firstPalindromic :: Int -> MaybeProd
firstPalindromic max =
  head (filter (\(p,x,y) -> palindromic p)
               (orderedPairs max))

otherMethod :: [Int]
otherMethod =
  [m
  |a <- [9]
  ,b <- [0 .. 9]
  ,c <- [0 .. 9]
  ,m <- [100001 * a + 10010 * b + 1100 * c]
  ,[x|x <- [100 .. 999],m `mod` x == 0 && m `div` x < 1000] /= []]

palindrome :: Int -> Bool
palindrome x =
  let digits = show x
  in digits == reverse digits

other2 :: Int
other2 = maximum [x * y|x <- [100 .. 999],y <- [100 .. 999],palindrome (x * y)]