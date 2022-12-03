main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map priority . commonLetters . chunksOfThree . lines

chunksOfThree :: [String] -> [(String, String, String)]
chunksOfThree [] = []
chunksOfThree (x:y:z:zs) = (x, y, z) : chunksOfThree zs

commonLetters :: [(String, String, String)] -> [Char]
commonLetters = map commonLetter

commonLetter :: (String, String, String) -> Char
-- Only iterates over c if first condition holds
commonLetter (a, b, c) = head [x | x <- a, y <- b, x == y, z <- c, y == z]

priority :: Char -> Int
priority c = maximum $ lookup c $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]
