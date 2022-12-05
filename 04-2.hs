main :: IO ()
main = interact solve

solve :: String -> String
solve =
  show .
  length .
  filter id .
  map
    ((isRedundant . (\x -> ((head x, x !! 1), (x !! 2, x !! 3)))) .
     (map (\s -> read s :: Int) . parse)) .
  lines

parse :: String -> [String]
parse =
  words .
  map
    (\x ->
       if x == ',' || x == '-'
         then ' '
         else x)

isRedundant :: ((Int, Int), (Int, Int)) -> Bool
isRedundant ((a, b), (c, d)) = (b >= c && a <= d) || (c <= b && d >= a)
