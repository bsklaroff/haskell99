import Data.Tree

main = putStrLn . show $ ipl testTree

testTree = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

ipl :: Tree a -> Int
ipl t = ipl' 0 t
  where ipl' n (Node a []) = n
        ipl' n (Node a xs) = n + sum (map (ipl' (n + 1)) xs)
