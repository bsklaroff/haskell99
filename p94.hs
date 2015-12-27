import Data.Maybe
import Data.List

main = putStrLn . show $ regular 6 3

data G a = Graph [a] [(a, a)] deriving (Show, Eq)

regular :: Int -> Int -> [G Int]
regular n k = nubBy iso $ regular' n k (Graph [] [])

regular' :: Int -> Int -> G Int -> [G Int]
regular' n k (Graph ns es)
  | length ns == n = [Graph ns es]
  | otherwise      = concatMap (regular' n k) (genGraphs n k ns es)

genGraphs :: Int -> Int -> [Int] -> [(Int, Int)] -> [G Int]
genGraphs n k ns es = map (\es0 -> Graph (ns ++ [n0]) (es ++ es0)) newEs
  where n0 = maximum (0:ns) + 1
        newEs = genEs n k n0 es

genEs :: Int -> Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
genEs n k n0 es = genEs' n0 (k - numEdges n0) openNs freeNs
  where numEdges nn = length $ filter (\(n1, n2) -> n1 == nn || n2 == nn) es
        openNs = filter ((\x -> x > 0 && x < k) . numEdges) [n0 + 1..n]
        freeNs = filter ((==0) . numEdges) [n0 + 1..n]

genEs' :: Int -> Int -> [Int] -> [Int] -> [[(Int, Int)]]
genEs' n0 k0 openNs freeNs = filter ((==k0) . length) $ map f chosenOpen
  where chosenOpen = concatMap (subsets openNs) (reverse [0..k0])
        f n1s = map (\n1 -> (n0, n1)) (n1s ++ (take (k0 - length n1s) freeNs))

subsets :: [Int] -> Int -> [[Int]]
subsets ns 0 = [[]]
subsets [] _ = []
subsets (n:ns) k = [n : x | x <- subsets ns (k - 1)] ++ subsets ns k

iso :: (Eq a, Ord a) => G a -> G a -> Bool
iso (Graph ns0 es0) (Graph ns1 es1) = any (==(Graph (sort ns1) (sort' es1))) $ map (permGraph ns0 es0) (perms ns0)

perms [] = [[]]
perms ns = concatMap (\n -> map (n:) (perms (filter (/=n) ns))) ns

permGraph oldNs oldEs newNs = Graph (sort oldNs) (sort' $ map (\(n0, n1) -> (pMap n0, pMap n1)) oldEs)
  where pMap n = oldNs !! fromJust (elemIndex n newNs)

sort' es = sort $ map (\(n0, n1) -> if n0 < n1 then (n0, n1) else (n1, n0)) es
