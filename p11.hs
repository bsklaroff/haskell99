import Data.List

main = putStrLn . show $ encodeModified "aaaabccaadeeee"

data RunLength a = Single a | Multiple Int a
                 deriving (Show)

encodeModified :: (Show a, Eq a) => [a] -> [RunLength a]
encodeModified [] = []
encodeModified xs =
    map (\xs@(x:_) -> if length xs == 1 then Single x else Multiple (length xs) x) (group xs)
