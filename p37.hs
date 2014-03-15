import Data.List
main = putStrLn . show $ totient 100900000

primes :: [Int]
primes = 2 : 3 : filter f [5,7..]
  where f x = all (\k -> mod x k /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors x = n : primeFactors (x `div` n)
  where n = head (filter (\k -> mod x k == 0) primes)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = foldr f [] . primeFactors
  where f x [] = [(x, 1)]
        f x (p@(y, k):ps)
          | x == y = (y, k + 1) : ps
          | otherwise = (x, 1) : p : ps

totient :: Int -> Int
totient n = foldr (\x p -> ((fst x) - 1) * (fst x) ^ ((snd x) - 1) * p) 1 (primeFactorsMult n)
