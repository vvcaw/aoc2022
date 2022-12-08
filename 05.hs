main :: IO ()
main = interact solve

solve :: String -> String
solve xs = show . map head $ foldl fold stacks movements
  where
    stacks = parseLists $ take 8 $ lines xs
    movements = drop 10 $ lines xs
    fold acc x = moveNCrates acc (parsed !! 1) (parsed !! 2) (head parsed)
      where
        parsed = parseMovement x

parseLists :: [String] -> [String]
parseLists xs
  | null (head xs) = []
  | otherwise =
    filter (/= ' ') (map (\x -> take 3 x !! 1) xs) :
    parseLists (map (drop 4) xs)

parseMovement :: String -> [Int]
parseMovement xs =
  (\x -> [asInt (x !! 1), asInt (x !! 3) - 1, asInt (x !! 5) - 1]) $ words xs
  where
    asInt x = read x :: Int

moveNCrates :: [String] -> Int -> Int -> Int -> [String]
moveNCrates xs from to 0 = xs
moveNCrates xs from to amount = moveNCrates crateMoved from to (amount - 1)
  where
    crateMoved = moveCrate xs from to

moveCrate :: [String] -> Int -> Int -> [String]
moveCrate xs from to
  | null fromStr = xs
  | otherwise = snd $ foldr switch (length xs - 1, []) xs
  where
    fromStr = xs !! from
    switch x (index, xss)
      | index == to = (index - 1, (head (xs !! from) : (xs !! to)) : xss)
      | index == from = (index - 1, drop 1 (xs !! from) : xss)
      | otherwise = (index - 1, x : xss)
