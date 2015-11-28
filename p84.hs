import Data.List

main = do putStrLn . show $ minspantree graph

graph = Graph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

data G a = Graph [a] [(a, a, a)] | Edges [(a, a, a)] deriving (Show, Eq)

minspantree :: (Eq a, Ord a) => G a -> G a
minspantree (Graph (n:ns) es) = Edges (minspantree' ns [] es)
minspantree' [] tree _ = sort tree
minspantree' _ _ [] = []
minspantree' nsLeft tree esLeft = minspantree' (filter (/=newN) nsLeft) (minEdge:tree) (filter (/=minEdge) esLeft)
  where (esToUse, esOther) = partition oneLeft esLeft
        minEdge = minimumBy (\(_, _, v0) (_, _, v1) -> compare v0 v1) esToUse
        getNewN e@(n0, n1, _) = if n0 `elem` nsLeft then n0 else n1
        newN = getNewN minEdge
        oneLeft e@(n0, n1, _) = (n0 `elem` nsLeft &&  n1 `notElem` nsLeft) || (n1 `elem` nsLeft && n0 `notElem` nsLeft)
