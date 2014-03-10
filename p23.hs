import System.Random

main = rnd_select "abcdefgh" 3 >>= putStrLn

rnd_select :: (Show a) => [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select _ 0 = return []
rnd_select xs k = rand_nums xs k >>= \nums -> return $ map (xs !!) nums
  where rand_nums xs k = getStdGen >>= return . take k . randomRs (0, length xs - 1)
