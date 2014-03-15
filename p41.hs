main = do
    putStrLn . show $ goldbachList 9 20
    putStrLn . show $ goldbachList' 4 2000 50

primes :: [Int]
primes = 2 : 3 : filter f [5,7..]
  where f x = all (\k -> mod x k /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)

goldbach :: Int -> (Int, Int)
goldbach x = goldbach' primes x
  where goldbach' ys@(p:ps) x
          | (x - p) `elem` (takeWhile (<= (x - p)) ys) = (p, x - p)
          | otherwise = goldbach' ps x

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach $ filter even [a..b]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b k = filter ((> k) . fst) (goldbachList a b)
