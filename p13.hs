import Data.List

main = putStrLn . show $ encodeDirect "aaaabccaadeeee"

data RunLength a = Single a | Multiple Int a
                 deriving (Show)

encodeDirect :: (Show a, Eq a) => [a] -> [RunLength a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' xs (Single x)
  where encodeDirect' [] ys = [ys]
        encodeDirect' (x:xs) (Single y) = if x == y
                                            then encodeDirect' xs (Multiple 2 y)
                                            else (Single y) : encodeDirect' xs (Single x)
        encodeDirect' (x:xs) (Multiple n y) = if x == y
                                                then encodeDirect' xs (Multiple (n+1) y)
                                                else (Multiple n y) : encodeDirect' xs (Single x)
