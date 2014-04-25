main = putStrLn . show $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right
  where mirror Empty Empty = True
        mirror (Branch _ ll lr) (Branch _ rl rr) = (mirror ll rr) && (mirror lr rl)
        mirror _ _ = False
