import Control.Monad.State

main = putStrLn . show $ layout tree65

tree65 = Branch 'n' (Branch 'k' (Branch 'c' (Branch 'a' Empty Empty) (Branch 'e' (Branch 'd' Empty Empty) (Branch 'g' Empty Empty))) (Branch 'm' Empty Empty)) (Branch 'u' (Branch 'p' Empty (Branch 'q' Empty Empty)) Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

layout :: Tree a -> Tree (a, (Int, Int))
layout t = layout' t (xleft t th) 1 th
  where th = height t
        height Empty = 0
        height (Branch _ l r) = 1 + max (height l) (height r)
        xleft (Branch _ Empty _) _ = 1
        xleft (Branch _ l _) h = 2^(h-2) + (xleft l (h-1))
        layout' Empty _ _ _ = Empty
        layout' (Branch v l r) x y h = Branch (v, (x, y)) l' r'
         where l' = layout' l (x-2^(h-2)) (y+1) (h-1)
               r' = layout' r (x+2^(h-2)) (y+1) (h-1)
