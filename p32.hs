import Data.List
main = putStrLn . show $ [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]

myGCD :: Int -> Int -> Int
myGCD x y
  | y == 0 = abs x
  | otherwise = myGCD y (mod x y)
