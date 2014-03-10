import System.Random
import Data.List

main = diff_select2 6 49 >>= putStrLn . show

diff_select :: Int -> Int -> IO [Int]
diff_select n m = getStdGen >>= diff_select' n [1..m]
  where diff_select' _ [] _ = return []
        diff_select' 0 _ _ = return []
        diff_select' n xs gen = diff_select' (n-1) ((take k xs)++(drop (k+1) xs)) nextGen >>= \ys -> return ((xs !! k):ys)
          where (k, nextGen) = randomR (0, length xs - 1) gen

diff_select2 :: Int -> Int -> IO [Int]
diff_select2 n m
  | n <= m = getStdGen >>= return . take n . nub . randomRs (1, m)
  | otherwise = error "Range not big enough"
