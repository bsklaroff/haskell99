main = putStrLn . show $ construct [3, 2, 5, 7, 1]

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct (x:xs) = Branch x (construct as) (construct bs)
  where as = filter (<=x) xs
        bs = filter (>x) xs
