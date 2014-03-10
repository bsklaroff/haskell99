main = putStrLn . show $ lfsort ["abc","de","fgh","de","ijkl","mn","o", "", "a", "abcdef"]

lfsort :: (Show a) => [[a]] -> [[a]]
lfsort xs = concatMap id (lfsort' (foldr groupLen [] xs))
  where groupLen x [] = [[x]]
        groupLen x (y@(k:_):ys) = if length k == length x then (x:y):ys else y:groupLen x ys
        lfsort' [] = []
        lfsort' (x:xs) = (lfsort' left) ++ ys ++ (lfsort' right)
          where (left, ys, right) = split xs x

split :: (Show a) => [[a]] -> [a] -> ([[a]], [[a]], [[a]])
split xs s = split' xs s ([], [s], [])
  where split' [] _ res = res
        split' (x:xs) s (ra, rb, rc)
          | length x < length s = split' xs s (ra ++ [x], rb, rc)
          | length x == length s = split' xs s (ra, rb ++ [x], rc)
          | length x > length s = split' xs s (ra, rb, rc ++ [x])
