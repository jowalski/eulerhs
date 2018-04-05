import System.Random
import Data.List (sortOn)
import Text.Printf

-- Problem 84: Monopoly odds
-- In the game, Monopoly, the standard board is set up in the following way:
--              GO   A1  CC1  A2  T1  R1  B1  CH1  B2   B3  JAIL
--              H2                                          C1
--              T2                                          U1
--              H1                                          C2
--              CH3                                         C3
--              R4                                          R2
--              G3                                          D1
--              CC3                                         CC2
--              G2                                          D2
--              G1                                          D3
--              G2J  F3  U2   F2  F1  R3  E3  E2   CH2  E1  FP
-- A player starts on the GO square and adds the scores on two 6-sided dice
-- to determine the number of squares they advance in a clockwise direction.
-- Without any further rules we would expect to visit each square with equal
-- probability: 2.5%. However, landing on G2J (Go To Jail), CC (community
-- chest), and CH (chance) changes this distribution.
-- In addition to G2J, and one card from each of CC and CH, that orders the
-- player to go to directly jail, if a player rolls three consecutive
-- doubles, they do not advance the result of their 3rd roll. Instead they
-- proceed directly to jail.
-- At the beginning of the game, the CC and CH cards are shuffled. When a
-- player lands on CC or CH they take a card from the top of the respective
-- pile and, after following the instructions, it is returned to the bottom
-- of the pile. There are sixteen cards in each pile, but for the purpose of
-- this problem we are only concerned with cards that order a movement; any
-- instruction not concerned with movement will be ignored and the player
-- will remain on the CC/CH square.
--   * Community Chest (2/16 cards):
--       1. Advance to GO
--       2. Go to JAIL
--   * Chance (10/16 cards):
--       1. Advance to GO
--       2. Go to JAIL
--       3. Go to C1
--       4. Go to E3
--       5. Go to H2
--       6. Go to R1
--       7. Go to next R (railway company)
--       8. Go to next R
--       9. Go to next U (utility company)
--      10. Go back 3 squares.
-- The heart of this problem concerns the likelihood of visiting a particular
-- square. That is, the probability of finishing at that square after a roll.
-- For this reason it should be clear that, with the exception of G2J for
-- which the probability of finishing on it is zero, the CH squares will have
-- the lowest probabilities, as 5/8 request a movement to another square, and
-- it is the final square that the player finishes at on each roll that we
-- are interested in. We shall make no distinction between "Just Visiting"
-- and being sent to JAIL, and we shall also ignore the rule about requiring
-- a double to "get out of jail", assuming that they pay to get out on their
-- next turn.
-- By starting at GO and numbering the squares sequentially from 00 to 39 we
-- can concatenate these two-digit numbers to produce strings that correspond
-- with sets of squares.
-- Statistically it can be shown that the three most popular squares, in
-- order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO
-- (3.09%) = Square 00. So these three most popular squares can be listed
-- with the six-digit modal string: 102400.
-- If, instead of using two 6-sided dice, two 4-sided dice are used, find the
-- six-digit modal string.
type Die = Int

type NDoubles = Int

type Roll = [Die]

type Deck = [Int]

type GameState = ([Roll],NDoubles,(Deck,Deck,Square))

data Square
  = GO
  | A Int
  | CC Int
  | T Int
  | R Int
  | B Int
  | CH Int
  | JAIL
  | C Int
  | U Int
  | D Int
  | FP
  | E Int
  | F Int
  | G2J
  | G Int
  | H Int
  deriving (Show,Eq)

board :: [Square]
board =
  [GO
  ,A 1
  ,CC 1
  ,A 2
  ,T 1
  ,R 1
  ,B 1
  ,CH 1
  ,B 2
  ,B 3
  ,JAIL
  ,C 1
  ,U 1
  ,C 2
  ,C 3
  ,R 2
  ,D 1
  ,CC 2
  ,D 2
  ,D 3
  ,FP
  ,E 1
  ,CH 2
  ,E 2
  ,E 3
  ,R 3
  ,F 1
  ,F 2
  ,U 2
  ,F 3
  ,G2J
  ,G 1
  ,G 2
  ,CC 3
  ,G 3
  ,R 4
  ,CH 3
  ,H 1
  ,T 2
  ,H 2]

pairs :: [a] -> [[a]]
pairs [] = []
pairs [x] = []
pairs (x1:x2:xs) = [x1,x2] : (pairs xs)

randomRolls :: (RandomGen g)
            => g -> Int -> Int -> [Roll]
randomRolls g d n = pairs . take (n * 2) $ (randomRs (1,d) g)

draw :: Eq a
     => Int -> [a] -> (a,[a])
draw n xs = (head bs,as ++ (tail bs))
  where (as,bs) = splitAt (n - 1) xs

randomDraws :: (RandomGen g)
            => g -> Int -> [Int] -> [Int]
randomDraws g 0 xs = []
randomDraws g n [] = []
randomDraws g n xs =
  x :
  (randomDraws newg
               (n - 1)
               ys)
  where (i,newg) = randomR (1,length xs) g
        (x,ys) = draw i xs

randomDeck :: (RandomGen g)
           => g -> [Int]
randomDeck g =
  randomDraws g
              16
              [1 .. 16]

nextPile :: Deck -> Deck
nextPile (c:cs) = cs ++ [c]

isDouble :: Roll -> Bool
isDouble [d1,d2] = d1 == d2

moveSpaces :: Int -> Square -> Square
moveSpaces n sq = (dropWhile (/= sq) (cycle board)) !! n'
  where n' =
          if (n < 0)
             then (n + length board)
             else n

--   * Community Chest (2/16 cards):
--       1. Advance to GO
--       2. Go to JAIL
--   * Chance (10/16 cards):
--       1. Advance to GO
--       2. Go to JAIL
--       3. Go to C1
--       4. Go to E3
--       5. Go to H2
--       6. Go to R1
--       7. Go to next R (railway company)
--       8. Go to next R
--       9. Go to next U (utility company)
--      10. Go back 3 squares.
drawCC :: Int -> Square -> Square
drawCC n sq =
  case n of
    1 -> GO
    2 -> JAIL
    _ -> sq

isR :: Square -> Bool
isR s =
  case s of
    R x -> True
    _ -> False

isU :: Square -> Bool
isU s =
  case s of
    U x -> True
    _ -> False

nextType :: (Square -> Bool) -> Square -> Square
nextType p sq = (head . dropWhile (not . p) . dropWhile (/= sq)) (cycle board)

drawCH :: Int -> Square -> Square
drawCH n sq =
  case n of
    1 -> GO
    2 -> JAIL
    3 -> C 1
    4 -> E 3
    5 -> H 2
    6 -> R 1
    7 -> nextType isR sq
    8 -> nextType isR sq
    9 -> nextType isU sq
    10 ->
      moveSpaces (-3)
                 sq
    _ -> sq

currSquare :: GameState -> Square
currSquare (_,_,(_,_,sq)) = sq

move :: GameState -> GameState
move (rolls,nds,(dcc,dch,sq)) =
  (nextRolls
  ,nnds
  ,if threeDoubles
      then (dcc,dch,JAIL)
      else case moveSpaces roll sq of
             G2J -> (dcc,dch,JAIL) -- go to jail
             CC x                  -- community chest card
              ->
               (nextPile dcc
               ,dch
               ,drawCC (head dcc)
                       (CC x))
             CH x               -- chance card
              ->
               (dcc
               ,nextPile dch
               ,drawCH (head dch)
                       (CH x))
             s -> (dcc,dch,s))  -- regular move
  where nextRolls = tail rolls
        roll = sum $ head rolls
        double = isDouble $ head rolls
        threeDoubles = double && nds == 2
        nnds =
          if double && (not threeDoubles)
             then (nds + 1)
             else 0

initGame :: (RandomGen g)
         => g -> Int -> Int -> GameState
initGame g d rolls =
  (randomRolls g1 d rolls,0,(randomDeck g2,randomDeck g3,GO))
  where (g1,g') = split g
        (g2,g3) = split g'

endGame :: GameState -> Bool
endGame ([],_,_) = True
endGame gs = False

recordSpaces
  :: GameState -> [(Square,Int)] -> [(Square,Int)]
recordSpaces state sqCnts =
  if endGame state
     then sqCnts
     else recordSpaces nextState
                       (countNs sqCnts sq)
  where nextState = move state
        (_,_,(_,_,sq)) = nextState

countNs :: Eq a
        => [(a,Int)] -> a -> [(a,Int)]
countNs [] x = [(x,1)]
countNs ((y,n):cnts) x =
  if x == y
     then ((y,n + 1) : cnts)
     else ((y,n) : (countNs cnts x))

boardToNum :: Square -> String
boardToNum sq =
  case lookup sq $ zip board [0 .. length board] of
    Nothing -> ""
    Just x -> printf "%02d" x

freqTable :: Int -> Int -> Int -> [(Square,Int)]
freqTable s d n =
  sortOn (negate . snd)
         (recordSpaces (initGame (mkStdGen s) d n)
                       [])

p84 :: String
p84 = (concat . map boardToNum . take 3 . fst . unzip) (freqTable 13 4 500000)