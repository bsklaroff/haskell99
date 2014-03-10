main = putStrLn . show $ myLength [2,399,47]

myLength :: (Integral a) => [x] -> a
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
