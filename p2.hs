main = putStrLn . show $ myButLast [1,2,3]

myButLast [x,y] = x
myButLast (x:xs) = myButLast xs
