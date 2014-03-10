import Data.List

main = putStrLn . show $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

data RunLength a = Single a | Multiple Int a
                 deriving (Show)

decodeModified :: (Show a, Eq a) => [RunLength a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
                          Single c -> c : decodeModified xs
                          Multiple n c -> take n (repeat c) ++ decodeModified xs
