import Data.Tree

main = do putStrLn . show $ nnodes testTree
          putStrLn . show $ stringToTree testStr
          putStrLn . show $ treeToString testTree

testTree = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
testStr = "afg^^c^bd^e^^^"

nnodes :: Tree a -> Int
nnodes (Node a []) = 1
nnodes (Node a xs) = 1 + foldr (\x acc -> acc + nnodes x) 0 xs

stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x (fst (stringToTree' xs))
  where stringToTree' (x:xs)
          | x == '^'  = ([], xs)
          | otherwise = ([Node x tree0] ++ tree1, rest1)
              where (tree0, rest0) = stringToTree' xs
                    (tree1, rest1) = stringToTree' rest0

treeToString :: Tree Char -> String
treeToString (Node x []) = x : "^"
treeToString (Node x ts) = x : foldl (\acc t -> acc ++ treeToString t) [] ts ++ "^"
