import Data.Tree

main = putStrLn . show $ bottomUp testTree

testTree = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

bottomUp :: Tree Char -> String
bottomUp (Node x []) = [x]
bottomUp (Node x ts) = (concat (map bottomUp ts)) ++ [x]
