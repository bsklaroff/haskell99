main = putStrLn . show $ hbalTree 'x' 3

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n = [Branch x left right | left <- xs, right <- ys] ++
               [Branch x left right | left <- ys, right <- xs] ++
               [Branch x left right | left <- ys, right <- ys]
  where xs = hbalTree x (n-2)
        ys = hbalTree x (n-1)
