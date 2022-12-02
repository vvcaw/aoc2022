import qualified Data.Text as T

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . fst . foldl findBiggestElf (0, 0) . lines
  where
    findBiggestElf (highest, acc) ""
      | acc > highest = (acc, 0)
      | otherwise = (highest, 0)
    findBiggestElf (highest, acc) numStr =
      let num = read numStr :: Integer
       in (highest, acc + num)
