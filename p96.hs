main = do putStrLn . show $ identifier "this-is-a-long-identifier"
          putStrLn . show $ identifier "a"
          putStrLn . show $ identifier "z0"
          putStrLn . show $ identifier "a-1-b"
          putStrLn . show $ identifier "this-ends-in-"
          putStrLn . show $ identifier "two--hyphens"
          putStrLn . show $ identifier ""
          putStrLn . show $ identifier "5"

identifier :: String -> Bool
identifier "" = False
identifier (x:xs)
  | x `elem` ['a'..'z'] = identifier' xs
  | otherwise           = False

identifier' :: String -> Bool
identifier' "" = True
identifier' (x:xs)
  | x `elem` alphaNum = identifier' xs
  | x =='-'           = postHyphen xs
  | otherwise         = False
    where alphaNum = ['a'..'z'] ++ ['0'..'9']
          postHyphen "" = False
          postHyphen (y:ys)
            | y `elem` alphaNum = identifier' ys
            | otherwise         = False

