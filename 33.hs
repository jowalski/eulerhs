-- Problem 33: Digit cancelling fractions
-- The fraction 49/98 is a curious fraction, as an inexperienced
-- mathematician in attempting to simplify it may incorrectly believe that
-- 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
-- If the product of these four fractions is given in its lowest common
-- terms, find the value of the denominator.
type Frac = ((Int,Int),(Int,Int))

fracs :: [Frac]
fracs = [(n,d)|n <- pairs,d <- pairs,pairToInt n < pairToInt d]
  where pairs = (,) <$> [1 .. 9] <*> [0 .. 9]

pairToInt :: (Int,Int) -> Int
pairToInt (n10,n) = (n10 * 10) + n

-- assume a > b
myGcd :: Int -> Int -> Int
myGcd a b =
  case mod' of
    0 -> b
    _ -> gcd b mod'
  where mod' = a `mod` b

reduceFrac :: Frac -> (Int,Int)
reduceFrac (n,d) = (n' `div` gcd',d' `div` gcd')
  where n' = pairToInt n
        d' = pairToInt d
        gcd' = myGcd d' n'

reduceSingleDigs :: (Int,Int) -> (Int,Int)
reduceSingleDigs (n,d)
  | n == 0 || d == 0 = (0,0)
  | otherwise = (n `div` gcd',d `div` gcd')
  where gcd' = myGcd d n

cancelDigs :: Frac -> [(Int,Int)]
cancelDigs ((n10,n1),(d10,d1)) =
  map reduceSingleDigs
      (cmp n10 d10 n1 d1 ++
       cmp n10 d1 n1 d10 ++ cmp n1 d10 n10 d1 ++ cmp n1 d1 n10 d10)
  where cmp a b c d =
          if a == b
             then [(c,d)]
             else []

isNotTrivial :: Frac -> Bool
isNotTrivial ((n10,n1),(d10,d1)) = not (n1 == 0 && d1 == 0)

isDigitCanceling :: Frac -> Bool
isDigitCanceling f =
  elem (reduceFrac f)
       (cancelDigs f) &&
  isNotTrivial f

prodFrac :: (Int,Int) -> (Int,Int) -> (Int,Int)
prodFrac (n1,d1) (n2,d2) = reduceSingleDigs (n1 * n2,d1 * d2)

p33 :: Int
p33 = d
  where (n,d) =
          foldl1 prodFrac (map reduceFrac (filter isDigitCanceling fracs))