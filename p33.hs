import Data.List
main = putStrLn . show $ coprime 35 64

myGCD :: Int -> Int -> Int
myGCD x y
  | y == 0 = abs x
  | otherwise = myGCD y (mod x y)

coprime :: Int -> Int -> Bool
coprime = ((==1) .) . myGCD
