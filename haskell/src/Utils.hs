module Utils (
    replaceChar
  , replaceJSQuotes
  , replaceElementInList
  ) where



replaceChar :: Char -> Char -> String -> String
replaceChar c nc = foldr (\x xs -> if x == c then nc:xs else x:xs) []

replaceElementInList :: [a] -> Int -> Maybe a -> [a]
replaceElementInList list number element =
  let (h,t) = splitAt number list in
    let dropped = drop 1 t in
      case element of
       Nothing -> h ++ dropped
       Just e  -> h ++ (e : dropped)


replaceJSQuotes :: String -> String
replaceJSQuotes = replaceChar '"' '\''
