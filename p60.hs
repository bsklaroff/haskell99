main = putStrLn . show . length $ hbalTreeNodes 'x' 15

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes n = 1 + (minNodes (n-1)) + (minNodes (n-2))

maxHeight :: Int -> Int
maxHeight n = subtract 1 . head $ filter ((>n) . minNodes) [1..]

minHeight :: Int -> Int
minHeight = ceiling . logBase 2 . fromIntegral . (+1)

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n = [Branch x left right | left <- xs, right <- ys] ++
               [Branch x left right | left <- ys, right <- xs] ++
               [Branch x left right | left <- ys, right <- ys]
  where xs = hbalTree x (n-2)
        ys = hbalTree x (n-1)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = filter ((==n) . nNodes) $ concatMap (hbalTree x) [minHeight n..maxHeight n]
  where nNodes Empty = 0
        nNodes (Branch _ left right) = 1 + (nNodes left) + (nNodes right)
