import Data.Tree

main = do putStrLn . show $ treeToLisp testTree
          putStrLn . show $ lispToTree testLisp

testLisp = "(a (f g) c (b d e))"
testTree = Node 'a' [ Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []]]

treeToLisp :: Tree Char -> String
treeToLisp (Node x []) = [x]
treeToLisp (Node x ts) = '(' : x : concatMap ((' ' :) . treeToLisp) ts ++ ")"

lispToTree :: String -> Tree Char
lispToTree t = (fst $ lispToTree' t) !! 0
  where lispToTree' ('(':x:xs) = ([Node x children], restTree)
          where (children, restTree) = parseChildren xs
        parseChildren (' ':'(':ys) = (subTree ++ otherChildren, otherRest)
              where (subTree, treeRest) = lispToTree' ('(':ys)
                    (otherChildren, otherRest) = parseChildren treeRest
        parseChildren (' ':y:ys) = ([Node y []] ++ otherChildren, otherRest)
              where (otherChildren, otherRest) = parseChildren ys
        parseChildren (')':ys) = ([], ys)
