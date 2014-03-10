main = putStrLn . show $ elementAt [2,399,47] 2

elementAt :: (Show a, Integral b) => [a] -> b -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) n
  | n >= 1 = elementAt xs (n-1)
  | otherwise = error "Index out of bounds"
