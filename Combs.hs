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
combswr xs n = [a : cs|(a,bs) <- allsplits xs,cs <- combswr bs (n - 1)]