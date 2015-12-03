import Data.List

main = do putStrLn . show $ depthfirst (Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1

data G a = Graph [a] [(a, a)] deriving (Show, Eq)

depthfirst :: (Eq a, Ord a) => G a -> a -> [a]
depthfirst (Graph ns es) startN = sort $ depthfirst' (Graph ns es) [] (Just startN)
  where depthfirst' _ res Nothing = res
        depthfirst' (Graph ns es) res (Just n)
          | n `elem` res = res
          | otherwise    = foldr (\otherN acc -> depthfirst' (Graph ns es) acc otherN) (n:res) (map (maybeOtherN n) es)
        maybeOtherN n (n0, n1)
          | n == n0   = Just n1
          | n == n1   = Just n0
          | otherwise = Nothing
