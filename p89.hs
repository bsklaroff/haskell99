import Data.List

main = do putStrLn . show $ bipartite (Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])

data G a = Graph [a] [(a, a)] deriving (Show, Eq)

bipartite :: (Eq a, Ord a) => G a -> Bool
bipartite (Graph ns es) = bipartite' [] ns
  where bipartite' _ [] = True
        bipartite' colors (n:rest)
          | nColor == 2 = False
          | otherwise   = bipartite' ((n, nColor):colors) rest
              where nColor = findColor n colors
        findColor n colors = foldr (\c acc -> if c == acc then acc + 1 else acc) 0 adjColors
          where adjColors = reverse $ sort [getColor (if n0 == n then n1 else n0) colors | (n0, n1) <- es, n0 == n || n1 == n]
                getColor n colors = case find ((==n) . fst) colors of
                  Just (_, color) -> color
                  Nothing -> -1
