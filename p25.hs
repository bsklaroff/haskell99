import System.Random
import Data.List

main = rnd_permu "abcdefg" >>= putStrLn . show

rnd_permu :: [a] -> IO [a]
rnd_permu xs = getStdGen >>= return . map (xs !!) . take len . nub . randomRs (0, len-1)
  where len = length xs
