main = putStrLn . show $ lsort ["abc","de","fgh","de","ijkl","mn","o"]

lsort :: (Show a) => [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = (lsort left) ++ ys ++ (lsort right)
    where (left, ys, right) = split xs x

split :: (Show a) => [[a]] -> [a] -> ([[a]], [[a]], [[a]])
split xs s = split' xs s ([], [s], [])
  where split' [] _ res = res
        split' (x:xs) s (ra, rb, rc)
          | length x < length s = split' xs s (ra ++ [x], rb, rc)
          | length x == length s = split' xs s (ra, rb ++ [x], rc)
          | length x > length s = split' xs s (ra, rb, rc ++ [x])
