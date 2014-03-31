import Data.List

main = putStrLn . show $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]

data Node = Node { value :: (Maybe Char, Int), leftChild :: Node, rightChild :: Node } | EmptyNode deriving (Show)

minNode :: [Node] -> Node
minNode xs = minNode' xs (maxBound, EmptyNode) where
  minNode' [] (_, n) = n
  minNode' (EmptyNode:xs) p = minNode' xs p
  minNode' (x@(Node (_, i) _ _):xs) p@(b, _) = minNode' xs p'
    where p' = if i < b then (i, x) else p

addNodes :: Node -> Node -> Node
addNodes x@(Node (_, xi) _ _) y@(Node (_, yi) _ _) = Node (Nothing, xi + yi) x y

buildTree :: [Node] -> Node
buildTree [] = EmptyNode
buildTree [x] = x
buildTree xs = buildTree ((addNodes y0 y1) : ys)
  where (y0:y1:ys) = sortBy f xs
          where f (Node (_, x) _ _) (Node (_, y) _ _) = compare x y

parseTree :: Node -> [(Char, [Char])]
parseTree x = parseTree' x ""
  where parseTree' (Node (Just c, _) EmptyNode EmptyNode) s = [(c, s)]
        parseTree' (Node _ left right) s = (parseTree' left (s ++ "0")) ++ (parseTree' right (s ++ "1"))

huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman xs =  parseTree $ buildTree (map f xs)
  where f (c, i) = Node (Just c, i) EmptyNode EmptyNode

