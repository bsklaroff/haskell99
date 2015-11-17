main = do let testTree = stringToTree testStr
              preorder = treeToPreorder testTree
              inorder = treeToInorder testTree
              preTree = preorderToTree preorder
              origTree = preInTree preorder inorder
          putStrLn . show $ "preorder: " ++ preorder
          putStrLn . show $ "inorder: " ++ inorder
          putStrLn . show $ "preTree: " ++ treeToString preTree
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

treeToPreorder :: Tree Char -> [Char]
treeToPreorder Empty = ""
treeToPreorder (Branch x left right) = x : treeToPreorder left ++ treeToPreorder right

treeToInorder :: Tree Char -> [Char]
treeToInorder Empty = ""
treeToInorder (Branch x left right) = treeToInorder left ++ x : treeToInorder right

preorderToTree :: [Char] -> Tree Char
preorderToTree "" = Empty
preorderToTree (x:xs) = Branch x Empty (preorderToTree xs)

preInTree :: [Char] -> [Char] -> Tree Char
preInTree "" "" = Empty
preInTree (x:xs) ys = Branch x (preInTree xl yl) (preInTree xr yr)
  where (xl, yl, xr, yr) = findSubs x xs ys

findSubs :: Char -> [Char] -> [Char] -> ([Char], [Char], [Char], [Char])
findSubs x xs ys = (preL, inL, preR, inR)
  where (z', inL, inR) = splitInorder x ys
        (preL, preR) = splitPreorder z' xs

splitInorder :: Char -> [Char] -> (Maybe Char, [Char], [Char])
splitInorder x (y:ys)
  | x == y    = (Nothing, "", ys)
splitInorder x (y:z:ys)
  | x == z    = (Just y, [y], ys)
  | otherwise = (z', y:inL, inR)
      where (z', inL, inR) = splitInorder x (z:ys)

splitPreorder :: Maybe Char -> [Char] -> ([Char], [Char])
splitPreorder Nothing xs = ("", xs)
splitPreorder (Just z) (x:xs)
  | z == x    = ([x], xs)
  | otherwise = (x:preL, preR)
      where (preL, preR) = splitPreorder (Just z) xs
