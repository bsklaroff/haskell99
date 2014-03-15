main = putStrLn . show $ goldbach 1000

primes :: [Int]
primes = 2 : 3 : filter f [5,7..]
  where f x = all (\k -> mod x k /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)

goldbach :: Int -> (Int, Int)
goldbach x = goldbach' primes x
  where goldbach' (p:ps) x
          | (x - p) `elem` (takeWhile (<= (x - p)) ps) = (p, x - p)
          | otherwise = goldbach' ps x
