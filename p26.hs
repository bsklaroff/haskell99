main = putStrLn . show $ combinations 3 "abcdef"

combinations :: (Show a, Eq a) => Int -> [a] -> [[a]]
combinations k xs
  | k < 0 || k > length xs = []
  | k == 0 = [[]]
  | otherwise = concatMap f xs
      where f x = map (x:) (combinations (k-1) (tail . (dropWhile (/= x)) $ xs))
