import Data.List
import Data.Maybe

main = do putStrLn . show $ iso graphG1 graphH1

data G a = Graph [a] [(a, a)] deriving (Show, Eq)

graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8), (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]

graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7), (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]

iso :: (Eq a, Ord a) => G a -> G a -> Bool
iso (Graph ns0 es0) (Graph ns1 es1) = any (==(Graph (sort ns1) (sort es1))) $ map (permGraph ns0 es0) (perms ns0)

perms [] = [[]]
perms ns = concatMap (\n -> map (n:) (perms (filter (/=n) ns))) ns

permGraph oldNs oldEs newNs = Graph (sort oldNs) (sort $ map (\(n0, n1) -> (pMap n0, pMap n1)) oldEs)
  where pMap n = oldNs !! fromJust (elemIndex n newNs)
