main = putStrLn . show $ myReverse [2,399,47]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
