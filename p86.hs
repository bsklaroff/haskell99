import Data.List

main = do putStrLn . show $ kcolor graph

data G a = Graph [a] [(a, a)] deriving (Show, Eq)

graph = Graph ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'] [('a', 'b'), ('a', 'e'), ('a', 'f'), ('b', 'c'), ('b', 'g'), ('c', 'd'), ('c', 'h'), ('d', 'e'), ('d', 'i'), ('e', 'j'), ('f', 'h'), ('f', 'i'), ('g', 'i'), ('g', 'j'), ('h', 'j')]

degree :: (Eq a, Ord a) => G a -> a -> Int
degree (Graph ns es) n = length (filter (\(n0, n1) -> n0 == n || n1 == n) es)

sortByDeg :: (Eq a, Ord a) => G a -> [a]
sortByDeg (Graph ns es) = sortBy (\n0 n1 -> compare (degree (Graph ns es) n1) (degree (Graph ns es) n0)) ns

kcolor :: (Eq a, Ord a) => G a -> [(a, Int)]
kcolor g = kcolor' g ns
  where ns = reverse $ sortByDeg g
        kcolor' (Graph _ es) xs = sort $ foldr (\n acc -> (n, minDeg n acc es) : acc) [] xs
        minDeg node cNodes edges = foldr (\e acc -> if e == acc then acc + 1 else acc) 1 (reverse . sort $ seenColors node cNodes edges)
        seenColors node cNodes edges = foldr (\e acc -> (colorVal node cNodes e) : acc) [] edges
        colorVal node cNodes (n0, n1)
          | n0 == node = getColor n1 cNodes
          | n1 == node = getColor n0 cNodes
          | otherwise  = 0
        getColor node cNodes = case find ((==node) . fst) cNodes of
          Just (_, color) -> color
          Nothing -> 0
