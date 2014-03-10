main = putStrLn . show $ split "abcdefghik" 3

split :: (Show a) => [a] -> Int -> ([a], [a])
split xs k = split' [] xs k
  where split' xs ys 0 = (xs, ys)
        split' xs (y:ys) k = split' (xs ++ [y]) ys (k-1)
        split' xs [] _ = (xs, [])
