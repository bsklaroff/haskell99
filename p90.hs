import Data.List

main = do putStrLn . show $ length $ queens 8

queens :: Int -> [[Int]]
queens n = queens' 1 []
  where queens' k xs
          | length xs == n  = [xs]
          | k == n + 1          = []
          | validBoard (k:xs) = queens' 1 (k:xs) ++ queens' (k + 1) xs
          | otherwise           = queens' (k + 1) xs

validBoard xs = (length $ nub xs) == length xs && checkDiags xs
  where checkDiags [] = True
        checkDiags (x:xs) = (/=0) $ foldl (\acc x0 -> if acc == 0 || x0 + acc == x || x0 - acc == x then 0 else acc + 1) 1 xs

