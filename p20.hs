main = putStrLn . show $ removeAt 4 ['a','b','c','d','e', 'f', 'g', 'h']

removeAt :: (Show a) => Int -> [a] -> (a, [a])
removeAt k xs
  | 0 < k && k <= length xs = (xs !! (k-1), (take (k-1) xs) ++ (drop k xs))
  | otherwise = error "Index out of bounds"
