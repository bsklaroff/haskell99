import Control.Monad.State

main = putStrLn . show $ layout tree65

tree65 = Branch 'n' (Branch 'k' (Branch 'c' (Branch 'a' Empty Empty) (Branch 'e' (Branch 'd' Empty Empty) (Branch 'g' Empty Empty))) (Branch 'm' Empty Empty)) (Branch 'u' (Branch 'p' Empty (Branch 'q' Empty Empty)) Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

zipRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
zipRanges [] _ = []
zipRanges _ [] = []
zipRanges ((_,x):xs) ((y,_):ys) = (x,y):(zipRanges xs ys)

nodeOffset :: [(Int, Int)] -> [(Int, Int)] -> Int
nodeOffset l r = foldr (\(x,y) i -> max i (((x - y) `div` 2) + 1)) 1 (zipRanges l r)

subRanges :: Tree a -> Tree (a, Int, [(Int, Int)])
subRanges Empty = Empty
subRanges (Branch v l r) = Branch (v, offset, (0,0):(mergeRanges sl' sr')) slt srt
  where slt = subRanges l
        srt = subRanges r
        sl = getRange slt
        sr = getRange srt
        getRange Empty = []
        getRange (Branch (_,_,rs) _ _) = rs
        offset = nodeOffset sl sr
        sl' = map (\(x, y) -> (x - offset, y - offset)) sl
        sr' = map (\(x, y) -> (x + offset, y + offset)) sr
        mergeRanges [] ys = ys
        mergeRanges xs [] = xs
        mergeRanges ((a, b):xs) ((c, d):ys) = ((min a c),(max b d)):(mergeRanges xs ys)

layout :: Tree a -> Tree (a, (Int, Int))
layout t = parseRanges (subRanges t)
  where parseRanges Empty = Empty
        parseRanges (Branch (v,o,rs) l r) = Branch (v, (x0, 1)) l' r'
         where x0 = foldr (\(a, _) i -> max i (-a + 1)) 1 rs
               l' = layout' l (x0 - o) 2
               r' = layout' r (x0 + o) 2
               layout' Empty _ _ = Empty
               layout' (Branch (vv,oo,_) ll rr) x y = Branch (vv, (x, y)) ll' rr'
                where ll' = layout' ll (x - oo) (y + 1)
                      rr' = layout' rr (x + oo) (y + 1)
