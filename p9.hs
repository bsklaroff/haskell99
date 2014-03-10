main = putStrLn . show $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'd', 'e', 'e', 'e', 'e']

pack :: (Show a, Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = pack' (x:xs) []
  where
    pack' [] ys = [ys]
    pack' (x:xs) [] = pack' xs [x]
    pack' (x:xs) (y:ys)
      | x == y = pack' xs (x:y:ys)
      | otherwise = (y:ys) : pack (x:xs)
