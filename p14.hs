import Data.List

main = putStrLn . show $ dupli [2,34,47,100]

dupli :: (Show a) => [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs
