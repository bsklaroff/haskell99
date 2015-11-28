import Data.List

main = do putStrLn . show $ paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
          putStrLn . show $ paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

data G a = Graph [a] [(a, a)] | Digraph [a] [(a, a)] | Adj [(a, [a])] | Edges [(a, a)] deriving (Show, Eq)

edgesToDigraph :: (Eq a, Ord a) => G a -> G a
edgesToDigraph (Edges es) = Digraph (nub . sort $ concatMap (\(a, b) -> [a, b]) es) (sort es)

digraphToAdj :: (Eq a, Ord a) => G a -> G a
digraphToAdj (Digraph ns es) = Adj (sort $ map findOpps ns)
  where findOpps n = (n, map snd (relevantEdges n))
        relevantEdges n = filter (\(n0, n1) -> n0 == n) es

edgesToAdj = digraphToAdj . edgesToDigraph

paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths src dest edgeList = map reverse $ paths' [] [src] (find ((==src) . fst) adjs)
  where Adj adjs = edgesToAdj (Edges edgeList)
        paths' _ _ Nothing = []
        paths' _ rs (Just (_, [])) = []
        paths' ss rs (Just (_, ns)) = extendPaths ss (map (:rs) ns)
        extendPaths seenList resList = concatMap (extendPath seenList) resList
        extendPath ys (x:xs)
          | x == dest = [(x:xs)]
          | x `elem` ys = []
          | otherwise = paths' (x:ys) (x:xs) (find ((==x) . fst) adjs)
