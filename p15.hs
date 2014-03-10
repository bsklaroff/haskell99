main = putStrLn . show $ repli [2,34,47,100] 3

repli :: (Show a) => [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) k = take k (repeat x) ++ repli xs k
