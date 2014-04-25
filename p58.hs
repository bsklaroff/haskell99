main = putStrLn . show $ symCbalTrees 5

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [leaf 'x']
cbalTree n
  | n `mod` 2 == 1 = [Branch 'x' t0 t1 | t0 <- trees0, t1 <- trees0]
  | otherwise = [Branch 'x' t0 t1 | t0 <- trees1, t1 <- trees2] ++
                [Branch 'x' t0 t1 | t0 <- trees2, t1 <- trees1]
  where trees0 = cbalTree ((n - 1) `div` 2)
        trees1 = cbalTree (n `div` 2)
        trees2 = cbalTree (n `div` 2 - 1)

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right
  where mirror Empty Empty = True
        mirror (Branch _ ll lr) (Branch _ rl rr) = (mirror ll rr) && (mirror lr rl)
        mirror _ _ = False

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree
