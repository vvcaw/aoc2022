data ElfCarry =
  ElfCarry
    { l :: Integer
    , m :: Integer
    , s :: Integer
    }

main :: IO ()
main = interact solve

solve :: String -> String
solve =
  show .
  (\c -> l c + m c + s c) .
  fst . foldl findBiggestElf (ElfCarry {l = 0, m = 0, s = 0}, 0) . lines
  where
    findBiggestElf (carry, acc) ""
      | acc > s carry = sortValues acc carry
      | otherwise = (carry, 0)
    findBiggestElf (carry, acc) numStr =
      let num = read numStr :: Integer
       in (carry, acc + num)

sortValues :: Integer -> ElfCarry -> (ElfCarry, Integer)
sortValues acc ElfCarry {l, m, s}
  | acc > l = (ElfCarry {s = m, m = l, l = acc}, 0)
  | acc > m = (ElfCarry {s = m, m = acc, l = l}, 0)
  | otherwise = (ElfCarry {s = acc, m = m, l = l}, 0)
