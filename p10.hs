main = putStrLn . show $ encode "aaaabccaadeeee"

encode :: (Show a, Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let (first, rest) = span (==x) xs
                    in (1 + length first, x) : encode rest
