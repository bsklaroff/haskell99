main = putStrLn . show $ insertAt 'X' ['a','b','c','d','e', 'f', 'g', 'h'] 2

insertAt :: (Show a) => a -> [a] -> Int -> [a]
insertAt x [] k
  | k == 1 = [x]
  | otherwise = error "Index out of bounds"
insertAt x (y:ys) k
  | k <= 0 = error "Index out of bounds"
  | k == 1 = x:y:ys
  | otherwise = y:insertAt x ys (k-1)
