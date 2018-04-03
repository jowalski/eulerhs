import Combs
import Data.List.Ordered (sort)

-- Problem 68: Magic 5-gon ring
-- Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6,
-- and each line adding to nine.
-- Working clockwise, and starting from the group of three with the
-- numerically lowest external node (4,3,2 in this example), each solution
-- can be described uniquely. For example, the above solution can be
-- described by the set: 4,3,2; 6,2,1; 5,1,3.
-- It is possible to complete the ring with four different totals: 9, 10, 11,
-- and 12. There are eight solutions in total.
--         Total          Solution Set
--         9              4,2,3; 5,3,1; 6,1,2
--         9              4,3,2; 6,2,1; 5,1,3
--         10             2,3,5; 4,5,1; 6,1,3
--         10             2,5,3; 6,3,1; 4,1,5
--         11             1,4,6; 3,6,2; 5,2,4
--         11             1,6,4; 5,4,2; 3,2,6
--         12             1,5,6; 2,6,4; 3,4,5
--         12             1,6,5; 3,5,4; 2,4,6
-- By concatenating each group it is possible to form 9-digit strings; the
-- maximum string for a 3-gon ring is 432621513.
-- Using the numbers 1 to 10, and depending on arrangements, it is possible
-- to form 16- and 17-digit strings. What is the maximum 16-digit string for
-- a "magic" 5-gon ring?
type Group3 = [Int]

type Ring = [Group3]

type Ring = [Group3]

listToRing3 :: [Int] -> Ring
listToRing3 l = [[li 0,li 1,li 2],[li 3,li 2,li 4],[li 5,li 4,li 1]]
  where li = (l !!)

listToRing5 :: [Int] -> Ring
listToRing5 l =
  [[li 0,li 1,li 2]
  ,[li 3,li 2,li 4]
  ,[li 5,li 4,li 6]
  ,[li 7,li 6,li 8]
  ,[li 9,li 8,li 1]]
  where li = (l !!)

allSumsEq :: [[Int]] -> Bool
allSumsEq (x:xs) = all ((==) (sum x) . sum) xs

restrict3 :: [Int] -> Bool
restrict3 comb = ci 0 < ci 3 && ci 0 < ci 5 && allSumsEq (listToRing3 comb)
  where ci = (comb !!)

restrict5 :: [Int] -> Bool
restrict5 comb =
  ci 0 < ci 3 &&
  ci 0 < ci 5 && ci 0 < ci 7 && ci 0 < ci 9 && allSumsEq (listToRing5 comb)
  where ci = (comb !!)

ring3s :: [Ring]
ring3s = (map listToRing3 . filter restrict3) (combswor [1 .. 6] 6)

ring5s :: [Ring]
ring5s = (map listToRing5 . filter restrict5) (combswor [1 .. 10] 10)

ringToStr :: Ring -> String
ringToStr = (concat . map (concat . map show))

maxRing :: [Ring] -> String
maxRing = (head . reverse . sort . map ringToStr)

p68 :: String
p68 = maxRing ring5s