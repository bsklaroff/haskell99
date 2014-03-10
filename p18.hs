main = putStrLn . show $ slice ['a','b','c','d','e', 'f', 'g', 'h', 'i', 'j', 'k'] 3 7

slice :: (Show a) => [a] -> Int -> Int -> [a]
slice [] _ _= []
slice (x:xs) a b
  | b == 0 = []
  | a == 1 = x : slice xs a (b-1)
  | otherwise = slice xs (a-1) (b-1)
