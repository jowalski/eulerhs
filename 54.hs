import Read (splitBy)
import Data.Char (ord)
import Data.List (sort, group, sortOn)

-- Problem 54: Poker hands
-- In the card game poker, a hand consists of five cards and are ranked, from
-- lowest to highest, in the following way:
--   * High Card: Highest value card.
--   * One Pair: Two cards of the same value.
--   * Two Pairs: Two different pairs.
--   * Three of a Kind: Three cards of the same value.
--   * Straight: All cards are consecutive values.
--   * Flush: All cards of the same suit.
--   * Full House: Three of a kind and a pair.
--   * Four of a Kind: Four cards of the same value.
--   * Straight Flush: All cards are consecutive values of same suit.
--   * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
-- The cards are valued in the order:
-- 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
-- If two players have the same ranked hands then the rank made up of the
-- highest value wins; for example, a pair of eights beats a pair of fives
-- (see example 1 below). But if two ranks tie, for example, both players
-- have a pair of queens, then highest cards in each hand are compared (see
-- example 4 below); if the highest cards tie then the next highest cards are
-- compared, and so on.
-- Consider the following five hands dealt to two players:
--         Hand   Player 1            Player 2              Winner
--         1      5H 5C 6S 7S KD      2C 3S 8S 8D TD        Player 2
--                Pair of Fives       Pair of Eights
--         2      5D 8C 9S JS AC      2C 5C 7D 8S QH        Player 1
--                Highest card Ace    Highest card Queen
--         3      2D 9C AS AH AC      3D 6D 7D TD QD        Player 2
--                Three Aces          Flush with Diamonds
--                4D 6S 9H QH QC      3D 6D 7H QD QS
--         4      Pair of Queens      Pair of Queens        Player 1
--                Highest card Nine   Highest card Seven
--                2H 2D 4C 4D 4S      3C 3D 3S 9S 9D
--         5      Full House          Full House            Player 1
--                With Three Fours    with Three Threes
-- The file poker.txt contains one-thousand random hands dealt to two players.
-- Each line of the file contains ten cards (separated by a single space): the
-- first five are Player 1's cards and the last five are Player 2's cards. You
-- can assume that all hands are valid (no invalid characters or repeated
-- cards), each player's hand is in no specific order, and in each hand there
-- is a clear winner.
-- How many hands does Player 1 win?
-- This problem references the following resources:
-- poker.txt
-- map (splitBy ' ')
readPoker :: FilePath -> IO [Round]
readPoker fp =
  readFile fp >>=
  (return . map (splitAt 5) . map (map strToCard) . map (splitBy ' ') . lines)

type Value = Int

data Suit
  = C
  | S
  | H
  | D
  deriving (Eq,Show)

type Card = (Value,Suit)

type Hand = [Card]

type FullRank = (Rank,Int,Int,Int,Int,Int)

data Rank
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  deriving (Eq,Ord,Show)

type Round = (Hand,Hand)

strToCard :: String -> Card
strToCard (v:s:[]) =
  (case v of
     'T' -> 10
     'J' -> 11
     'Q' -> 12
     'K' -> 13
     'A' -> 14
     _ -> ord v - ord '1' + 1
  ,case s of
     'S' -> S
     'C' -> C
     'H' -> H
     'D' -> D)

suit :: Card -> Suit
suit = snd

value :: Card -> Value
value = fst

allSame :: Eq a
        => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

pad :: Int -> a -> [a] -> [a]
pad len val xs
  | length xs == len = xs
  | otherwise = pad len val (xs ++ [val])

fullRank :: Rank -> [Int] -> [FullRank]
fullRank r ns = [(r,n1,n2,n3,n4,n5)]
  where n1:n2:n3:n4:n5:[] = pad 5 0 ns

values :: Hand -> [Int]
values = reverse . sort . map value

straightFlush :: Hand -> [FullRank]
straightFlush h =
  if (allSame . map suit) h && isInARow vh
     then (fullRank StraightFlush (values h))
     else []
  where vh = values h

flush :: Hand -> [FullRank]
flush h
  | (allSame . map suit) h = fullRank Flush (values h)
  | otherwise = []

isInARow :: (Enum a
            ,Eq a)
         => [a] -> Bool
isInARow [] = True
isInARow [_] = True
isInARow (x1:x2:xs) = succ x2 == x1 && isInARow (x2 : xs)

straight :: Hand -> [FullRank]
straight h =
  if isInARow h'
     then fullRank Straight h'
     else []
  where h' = values h

nKind :: Hand -> [Int]
nKind h = nk : val : rest
  where vgh = valGrp h
        nk = (maximum . map length) vgh
        val = (head . head . filter ((== nk) . length)) vgh
        rest =
          (map head . sortOn (negate . length) . filter ((/= val) . head)) vgh

kind :: Int -> Hand -> [FullRank]
kind n h =
  if n /= head nk
     then []
     else fullRank (case n of
                      4 -> FourKind
                      3 -> ThreeKind
                      2 -> OnePair
                      1 -> HighCard)
                   (tail nk)
  where nk = nKind h

valGrp :: Hand -> [[Int]]
valGrp = sortOn length . group . values

fullHouse :: Hand -> [FullRank]
fullHouse h =
  if ((== [2,3]) . map length . valGrp) h
     then fullRank FullHouse ((tail . nKind) h)
     else []

twoPair :: Hand -> [FullRank]
twoPair h =
  if ((== [1,2,2]) . map length . valGrp) h
     then fullRank TwoPair ((tail . nKind) h)
     else []

rank :: Hand -> FullRank
rank h =
  (head . concat)
    (map (\hType -> hType h)
         [straightFlush
         ,kind 4
         ,fullHouse
         ,flush
         ,straight
         ,kind 3
         ,twoPair
         ,kind 2
         ,kind 1])

rankRound :: Round -> (FullRank,FullRank)
rankRound (h1,h2) = (rank h1,rank h2)

win :: Round -> Int
win (h1,h2) =
  if (rank h1 > rank h2)
     then 1
     else 0

p54 :: IO Int
p54 = readPoker "poker.txt" >>= (return . sum . map win)