import Data.List

main = do putStrLn . show $ adjToGraph (Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")])
          putStrLn . show $ graphToAdj (Graph "bcdfghk" [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])
          putStrLn . show $ graphToDigraph (Graph "bcdfghk" [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])
          putStrLn . show $ digraphToGraph (Digraph "bcdfghk" [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ adjToDigraph (Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")])
          putStrLn . show $ digraphToAdj (Digraph "bcdfghk" [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ edgesToDigraph (Edges [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ digraphToEdges (Digraph "bcdfghk" [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ edgesToGraph (Edges [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ graphToEdges (Graph "bcdfghk" [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])
          putStrLn . show $ edgesToAdj (Edges [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')])
          putStrLn . show $ adjToEdges (Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")])

data G a = Graph [a] [(a, a)] | Digraph [a] [(a, a)] | Adj [(a, [a])] | Edges [(a, a)] deriving (Show, Eq)

-- Assumes Adj represents a valid undirected graph
adjToGraph :: (Eq a, Ord a) => G a -> G a
adjToGraph (Adj xs) = Graph (sort (map fst xs)) edges
  where edges = sort (nub allEdges)
        allEdges = concatMap (\(n, ns) -> map (\n0 -> if n < n0 then (n, n0) else (n0, n)) ns) xs

graphToAdj :: (Eq a, Ord a) => G a -> G a
graphToAdj (Graph ns es) = Adj (sort $ map findOpps ns)
  where findOpps n = (n, map (\(n0, n1) -> if n0 == n then n1 else n0) (relevantEdges n))
        relevantEdges n = filter (\(n0, n1) -> n0 == n || n1 == n) es

graphToDigraph :: (Eq a, Ord a) => G a -> G a
graphToDigraph (Graph ns es) = Digraph ns (sort $ concatMap (\(a, b) -> [(a, b), (b, a)]) es)

-- Assumes Digraph represents a valid undirected graph
digraphToGraph :: (Eq a, Ord a) => G a -> G a
digraphToGraph (Digraph ns es) = Graph ns (sort . nub $ map (\(a, b) -> if a < b then (a, b) else (b, a)) es)

adjToDigraph :: (Eq a, Ord a) => G a -> G a
adjToDigraph (Adj xs) = Digraph (sort (map fst xs)) (sort $ concatMap (\(n, ns) -> map (\n0 -> (n, n0)) ns) xs)

digraphToAdj :: (Eq a, Ord a) => G a -> G a
digraphToAdj (Digraph ns es) = Adj (sort $ map findOpps ns)
  where findOpps n = (n, map snd (relevantEdges n))
        relevantEdges n = filter (\(n0, n1) -> n0 == n) es

edgesToDigraph :: (Eq a, Ord a) => G a -> G a
edgesToDigraph (Edges es) = Digraph (nub . sort $ concatMap (\(a, b) -> [a, b]) es) (sort es)

digraphToEdges :: (Eq a, Ord a) => G a -> G a
digraphToEdges (Digraph ns es) = Edges (sort es)

edgesToGraph = digraphToGraph . edgesToDigraph
edgesToAdj = digraphToAdj . edgesToDigraph
graphToEdges = digraphToEdges . graphToDigraph
adjToEdges = digraphToEdges . adjToDigraph
