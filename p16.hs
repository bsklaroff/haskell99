main = putStrLn . show $ dropEvery [2,34,47,100] 1

dropEvery :: (Show a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs k = take (k-1) xs ++ dropEvery (drop k xs) k
