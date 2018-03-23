-- Problem 17: Number letter counts
-- If the numbers 1 to 5 are written out in words: one, two, three, four,
-- five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written
-- out in words, how many letters would be used?
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
-- 20 letters. The use of "and" when writing out numbers is in compliance
-- with British usage.
data NType
  = Teen
  | Tens

four :: NType -> String
four Teen = "four"
four Tens = "for"

nprefixes :: NType -> [String]
nprefixes nt = ["thir",four nt,"fif","six","seven","eigh","nine"]

base :: [String]
base = ["","one","two","three","four","five","six","seven","eight","nine"]

teens :: [String]
teens = ["ten","eleven","twelve"] ++ (map (++ "teen") (nprefixes Teen))

tens :: [String]
tens = map (++ "ty") (["twen"] ++ (nprefixes Tens))-- (++) <$> tens <*> base

firstHundred :: [String]
firstHundred = base ++ teens ++ ((++) <$> tens <*> base)

hundreds :: [String]
hundreds = map (++ "hundred") (tail base)

dropLastAnd :: String -> String
dropLastAnd s =
  case last3 of
    "and" -> take len s
    _ -> s
  where len = (length s) - 3
        last3 = drop len s

-- sum (map length firstThous)
firstThous :: [String]
firstThous =
  map dropLastAnd
      (firstHundred ++ ((++) <$> map (++ "and") hundreds <*> firstHundred)) ++
  ["onethousand"]