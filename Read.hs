module Read where

splitBy :: Char -> String -> [String]
splitBy c s =
  case break (== c) s of
    (s1,',':rest) -> s1 : (splitBy c rest)
    (s1,"") -> [s1]

unquote :: String -> String
unquote s =
  case s of
    '"':xs ->
      case reverse xs of
        '"':ys -> reverse ys
        _ -> s
    _ -> s

readCommaQuotes :: FilePath -> IO [String]
readCommaQuotes fp =
  readFile fp >>= (return . (map unquote) . (splitBy ',') . head . lines)