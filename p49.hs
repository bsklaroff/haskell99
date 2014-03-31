main = putStrLn . show $ gray 4

gray :: Int -> [[Char]]
gray 0 = [""]
gray n = map ('0':) prev ++ map ('1':) (reverse prev)
  where prev = gray (n - 1)
