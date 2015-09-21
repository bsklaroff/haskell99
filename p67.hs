main = do putStrLn . show $ stringToTree testStr
          putStrLn . show $ treeToString testTree
          putStrLn . show $ (treeToString (stringToTree testStr)) == testStr

testStr = "x(y(z,),a(,b))"
testTree = Branch 'x' (Branch 'y' (Branch 'z' Empty Empty) Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

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
