main = do let testTree = stringToTree testStr
              ds = tree2ds testTree
              origTree = ds2tree ds
          putStrLn . show $ "ds: " ++ ds
          putStrLn . show $ "origTree: " ++ treeToString origTree

testStr = "a(b(d,e),c(,f(g,)))"

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

stringToTree :: [Char] -> Tree Char
stringToTree "" = Empty
stringToTree (x:xs) = if x == ')' || x == ',' then Empty else Branch x (leftSubtree xs) (rightSubtree xs 0)

leftSubtree :: [Char] -> Tree Char
leftSubtree (x:xs) = if x == '(' then stringToTree xs else Empty

rightSubtree:: [Char] -> Int -> Tree Char
rightSubtree "" n = Empty
rightSubtree (x:xs) n
  | n == 0 && x == ',' = Empty
  | n == 1 && x == ',' = stringToTree xs
  | x == '('           = rightSubtree xs (n + 1)
  | x == ')'           = rightSubtree xs (n - 1)
  | otherwise          = rightSubtree xs n

treeToString :: Tree Char -> [Char]
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x left right) = x : "(" ++ treeToString left ++ "," ++ treeToString right ++ ")"

ds2tree :: [Char] -> Tree Char
ds2tree (x:xs)
  | x == '.'  = Empty
  | otherwise = Branch x (ds2tree xs) (ds2tree (findRightSub xs 1))
      where findRightSub (y:ys) n
              | n == 0    = y:ys
              | y == '.'  = findRightSub ys (n - 1)
              | otherwise = findRightSub ys (n + 1)

tree2ds :: Tree Char -> [Char]
tree2ds Empty = "."
tree2ds (Branch x left right) = x : tree2ds left ++ tree2ds right
