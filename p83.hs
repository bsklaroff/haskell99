import Data.List

main = do putStrLn . show $ spantree k4

k4 = Graph "abcd" [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

data G a = Graph [a] [(a, a)] | Digraph [a] [(a, a)] | Adj [(a, [a])] | Edges [(a, a)] deriving (Show, Eq)

spantree :: (Eq a, Ord a) => G a -> [G a]
spantree (Graph (n:ns) es) = map Edges (spantree' ns [] es)
spantree' [] tree _ = [sort tree]
spantree' _ _ [] = []
spantree' nsLeft tree esLeft = concatMap useEdge esToUse
  where (esToUse, esOther) = partition oneLeft esLeft
        oneLeft e@(n0, n1) = (n0 `elem` nsLeft &&  n1 `notElem` nsLeft) || (n1 `elem` nsLeft && n0 `notElem` nsLeft)
        useEdge e@(n0, n1) = spantree' (filter (/=newN) nsLeft) (e:tree) ((drop 1 $ dropWhile (/=e) esToUse) ++ esOther)
          where newN = if n0 `elem` nsLeft then n0 else n1
