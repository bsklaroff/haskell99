import Data.List as List
import Data.HashMap as Map
import Data.HashSet as Set
import Data.Maybe

main = do putStrLn . show $ head $ knightsTo 8 (1,1)
          putStrLn . show $ head $ closedKnights 8

jumps :: Int -> (Int, Int) -> Set (Int, Int)
jumps n (x, y) = Set.filter (onBoard n) poss
  where onBoard n0 (x0, y0) = x0 >= 1 && x0 <= n && y0 >=1 && y0 <= n
        poss = foldr (\x acc -> Set.insert x acc) Set.empty [(x+1,y+2),(x+1,y-2),(x-1,y+2),(x-1,y-2),(x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2,y-1)]

initJumps :: Int -> Map (Int, Int) (Set (Int, Int))
initJumps n = foldr (\x acc -> Map.insert x (jumps n x) acc) Map.empty [(x, y) | x <- [1..n], y <- [1..n]]

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n w = knightsTo' (initJumps n) [w]
  where knightsTo' neighborsLeft res@(u:_)
          | length res == n * n = [res]
          | neighborsValid      = concatMap (\v -> knightsTo' newNeighborsLeft (v:res)) toVisit
          | otherwise           = []
              where neighbors = Set.toList $ neighborsLeft ! u
                    newNeighborsLeft = foldr (\x acc -> Map.update (\v -> Just (Set.delete u v)) x acc) (Map.delete u neighborsLeft) neighbors
                    desperateNeighbors = List.filter ((==0) . Set.size . snd) $ List.map (\x -> (x, newNeighborsLeft ! x)) neighbors
                    neighborsValid = (<=1) . length $ desperateNeighbors
                    toVisit = if length desperateNeighbors > 0 then List.map fst desperateNeighbors else neighbors

closedKnights :: Int -> [[(Int, Int)]]
closedKnights n = concatMap (closedKnights' (initJumps n)) [[start] | start <- Set.toList $ jumps n (1, 1)]
  where closedKnights' neighborsLeft res@(u:_)
          | length res == n * n && u == (1,1) = [res]
          | u == (1, 1)                       = []
          | startValid && neighborsValid = concatMap (\v -> closedKnights' newNeighborsLeft (v:res)) toVisit
          | otherwise                    = []
              where neighbors = Set.toList $ neighborsLeft ! u
                    newNeighborsLeft = foldr (\x acc -> Map.update (\v -> Just (Set.delete u v)) x acc) (Map.delete u neighborsLeft) neighbors
                    desperateNeighbors = List.filter ((==0) . Set.size . snd) $ List.map (\x -> (x, newNeighborsLeft ! x)) neighbors
                    neighborsValid = (<=1) . length $ desperateNeighbors
                    startValid = length res == n * n - 1 || ((/=0) . Set.size $ newNeighborsLeft ! (1,1))
                    toVisit = if length desperateNeighbors > 0 then List.map fst desperateNeighbors else neighbors
