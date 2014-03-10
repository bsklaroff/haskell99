import Data.List
main = putStrLn . show $ totient 10

myGCD :: Int -> Int -> Int
myGCD x y
  | y == 0 = abs x
  | otherwise = myGCD y (mod x y)

coprime :: Int -> Int -> Bool
coprime = ((==1) .) . myGCD

totient :: Int -> Int
totient x = length $ filter (coprime x) [1..x]
