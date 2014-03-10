import Data.List
main = putStrLn . show $ isPrime 5915587277

isPrime :: Integer -> Bool
isPrime x = length factors == 0
  where factors = filter (\f -> mod x f == 0) [2..m]
        m = ceiling . sqrt . fromIntegral $ x
