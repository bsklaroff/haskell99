main = putStrLn . show . length $ group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

combinations :: (Show a, Eq a) => Int -> [a] -> [([a], [a])]
combinations k xs = combinations' k xs []
  where combinations' k xs ys
          | k == 0 = [([], ys ++ xs)]
          | k < 0 || xs == [] = []
        combinations' k (x:xs) ys = (map (\(a, b) -> (x:a, b)) (combinations' (k-1) xs ys)) ++ (combinations' k xs (ys++[x]))

group :: (Show a, Eq a) => [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (x:xs) ys = concatMap (\(a, b) -> map (a:) (group xs b)) (combinations x ys)
