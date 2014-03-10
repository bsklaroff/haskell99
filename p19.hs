main = do
    putStrLn . show $ rotate ['a','b','c','d','e', 'f', 'g', 'h'] 3
    putStrLn . show $ rotate ['a','b','c','d','e', 'f', 'g', 'h'] (-2)

rotate :: (Show a) => [a] -> Int -> [a]
rotate xs k
  | k > length xs = xs
  | k >= 0 = (drop k xs) ++ (take k xs)
  | k < 0 = let n = length xs + k in (drop n xs) ++ (take n xs)
