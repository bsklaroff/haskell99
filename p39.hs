main = putStrLn . show $ primesR 100 200

primes :: [Int]
primes = 2 : 3 : filter f [5,7..]
  where f x = all (\k -> mod x k /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)

primesR :: Int -> Int -> [Int]
primesR a b  = takeWhile (<= b) $ dropWhile (< a) primes
