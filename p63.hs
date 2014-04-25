main = putStrLn . show $ completeBinaryTree 4
--main = putStrLn . show $ isCompleteBinaryTree (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty))
--main = putStrLn . show $ isCompleteBinaryTree (completeBinaryTree 10)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = cbt' n 1
  where cbt' n a
         | a > n = Empty
         | otherwise = Branch 'x' (cbt' n (2*a)) (cbt' n (2*a+1))

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree t = icbt' t 1 (numNodes t)
  where icbt' Empty k n = k > n
        icbt' (Branch _ l r) k n = k <= n && icbt' l (2*k) n && icbt' r (2*k+1) n
        numNodes Empty = 0
        numNodes (Branch _ l r) = 1 + numNodes l + numNodes r

