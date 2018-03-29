module Combs where

import Data.List (sort)

takeNth :: [a] -> Int -> (a,[a])
takeNth xs n = (head bs,as ++ (tail bs))
  where (as,bs) = splitAt n xs

combsWOReplace :: [a] -> Int -> [[a]]
combsWOReplace [] _ = [[]]
combsWOReplace _ 0 = [[]]
combsWOReplace xs n =
  [c
  |(x,ds) <-
     map (takeNth xs)
         [0 .. length xs - 1]
  ,c <-
     map ([x] ++)
         (combsWOReplace ds
                         (n - 1))]

combsWReplace :: [a] -> Int -> [[a]]
combsWReplace [] _ = [[]]
combsWReplace _ 0 = [[]]
combsWReplace xs n =
  [c
  |x <-
     map ((!!) xs)
         [0 .. length xs - 1]
  ,c <-
     map ([x] ++)
         (combsWReplace xs
                        (n - 1))]

combsInOrder :: Ord a
             => [a] -> Int -> [[a]]
combsInOrder [] _ = [[]]
combsInOrder _ 0 = [[]]
combsInOrder xs n =
  [c
  |ds <-
     map (\i -> drop i objs)
         [0 .. length objs - 1]
  ,c <-
     map ([head ds] ++)
         (combsInOrder ds
                       (n - 1))]
  where objs = sort xs

allsplits :: [a] -> [(a,[a])]
allsplits xs =
  map (takeNth xs)
      [0 .. length xs - 1]

combswr :: [a] -> Int -> [[a]]
combswr [] _ = [[]]
combswr _ 0 = [[]]
combswr [x] _ = [[x]]
combswr xs n =
  [a : cs|a <- map (xs !!) [0 .. length xs - 1],cs <- combswr xs (n - 1)]

-- combinations without replacement, all orders
combswor :: [a] -> Int -> [[a]]
combswor [] _ = [[]]
combswor _ 0 = [[]]
combswor [x] _ = [[x]]
combswor xs n = [a : cs|(a,bs) <- allsplits xs,cs <- combswor bs (n - 1)]

-- combinations without replacement, in order
combsworo :: [a] -> Int -> [[a]]
combsworo [] _ = [[]]
combsworo _ 0 = [[]]
combsworo [x] _ = [[x]]
combsworo xs n =
  [last as : cs
  |(as,bs) <-
     map (\n -> splitAt n xs)
         [1 .. length xs - n + 1]
  ,cs <- combsworo bs (n - 1)]