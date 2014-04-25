import Control.Monad.State

main = putStrLn . show $ layout tree64

tree64 = Branch 'n' (Branch 'k' (Branch 'c' (Branch 'a' Empty Empty) (Branch 'h' (Branch 'g' (Branch 'e' Empty Empty) Empty) Empty)) (Branch 'm' Empty Empty)) (Branch 'u' (Branch 'p' Empty (Branch 's' (Branch 'q' Empty Empty) Empty)) Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

layout :: Tree a -> Tree (a, (Int, Int))
layout t = snd (layout' t 1 1)
  where layout' Empty _ _ = (0, Empty)
        layout' (Branch v l r) x y = (lx+rx+1, Branch (v, (x+lx, y)) lb rb)
         where (lx, lb) = layout' l x (y+1)
               (rx, rb) = layout' r (x+lx+1) (y+1)
