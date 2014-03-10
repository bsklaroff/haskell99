main = putStrLn . show $ myLast [1,2,3]

myLast [x] = x
myLast (x:xs) = myLast xs
