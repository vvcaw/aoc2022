main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (priority . commonLetter . splitInHalf) . lines

splitInHalf :: String -> (String, String)
splitInHalf l = splitAt (length l `div` 2) l

commonLetter :: (String, String) -> Char
commonLetter (a, b) = head [x | x <- a, y <- b, x == y]

priority :: Char -> Int
priority c = maximum $ lookup c $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]
