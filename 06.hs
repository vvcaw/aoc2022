main :: IO ()
main = interact solve

solve :: String -> String
solve = show . windowNotEq 0

windowNotEq :: Int -> String -> Int
windowNotEq pos (a:b:c:d:ds) =
  if (a /= b) && (a /= c) && (a /= d) && (b /= c) && (b /= d) && (c /= d)
    then pos + 4 -- Offset 3 as we count the position of a
    else windowNotEq (pos + 1) (b : c : d : ds)
