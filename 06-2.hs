main :: IO ()
main = interact solve

solve :: String -> String
solve = show . fst . foldl windowNotEq (0, "")

windowNotEq :: (Int, String) -> Char -> (Int, String)
windowNotEq (pos, cache) v =
  if allDifferent cache && length cache == 14
    then (pos, cache)
    else (pos + 1, updatedCache cache ++ [v])
  where
    updatedCache cache
      | length cache < 14 = cache
      | length cache == 14 = tail cache
    allDifferent str =
      case str of
        [] -> True
        (x:xs) -> x `notElem` xs && allDifferent xs
