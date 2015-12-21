main = do putStrLn . show $ head $ vonKoch [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]

vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch es = map reverse $ vonKoch' (maxNum es) es []

vonKoch' :: Int -> [(Int, Int)] -> [Int] -> [[Int]]
vonKoch' k es res
  | length res == k = [res]
  | otherwise       = concatMap (vonKoch' k es) $ filter (valid es) (genNext k res)

maxNum :: [(Int, Int)] -> Int
maxNum = foldr (\(n0, n1) acc -> max (max n0 acc) n1) 0

valid :: [(Int, Int)] -> [Int] -> Bool
valid es res = valid' es res []
  where valid' [] res seenEs = True
        valid' (e:es) res seenEs
          | eDiff == -1         = valid' es res seenEs
          | eDiff `elem` seenEs = False
          | otherwise           = valid' es res (eDiff:seenEs)
              where eDiff = findEDiff e res

findEDiff :: (Int, Int) -> [Int] -> Int
findEDiff (n0, n1) ns
  | length ns < max n0 n1 = -1
  | otherwise             = abs $ (findNVal (length ns - n0) ns) - (findNVal (length ns - n1) ns)

findNVal :: Int -> [Int] -> Int
findNVal 0 (n:ns) = n
findNVal k (n:ns) = findNVal (k - 1) ns

genNext :: Int -> [Int] -> [[Int]]
genNext k []
  | k == 0    = []
  | otherwise = [[1]]
genNext k (n:rest) = appendNext ++ incN
  where appendNext = findNextN k 1 (n:rest)
        incN = findNextN k (n + 1) rest

findNextN :: Int -> Int -> [Int] -> [[Int]]
findNextN k n rest
  | n > k         = []
  | n `elem` rest = findNextN k (n + 1) rest
  | otherwise     = [n:rest]
