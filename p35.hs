import Data.List
main = putStrLn . show $ primeFactors 315

primes :: [Int]
primes = 2 : 3 : filter f [5,7..]
  where f x = all (\k -> mod x k /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors x = n : primeFactors (x `div` n)
  where n = head (filter (\k -> mod x k == 0) primes)
