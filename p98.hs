main = putStrLn $ nonogram [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]

nonogram :: [[Int]] -> [[Int]] -> String
nonogram rows cols = solutionToStr rows cols $ solve rows cols

solve :: [[Int]] -> [[Int]] -> [[Int]]
solve rows cols = reverse $ solve' rows cols []

solve' :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
solve' rows cols res
  | length res == length rows = res
  | addSol /= []              = addSol
  | moveSol /= []             = moveSol
  | otherwise                 = []
      where addRow = nextValid rows cols res []
            moveRow = if length res > 0 then nextValid rows cols (tail res) (head res) else []
            addSol = if addRow == [] then [] else solve' rows cols (addRow:res)
            moveSol = if moveRow == [] then [] else solve' rows cols (moveRow:(tail res))

nextValid :: [[Int]] -> [[Int]] -> [[Int]] -> [Int] -> [Int]
nextValid rows cols res cur
  | rowTry == []                           = []
  | valid rows cols (reverse (rowTry:res)) = rowTry
  | otherwise                              = nextValid rows cols res rowTry
      where curRow = rows !! (length res)
            rowTry = nextTry (length cols) curRow cur

nextTry :: Int -> [Int] -> [Int] -> [Int]
nextTry width (n:ns) []
  | width < n  = []
  | null ns    = [0]
  | rest /= [] = 0 : map f rest
  | otherwise  = []
      where rest = nextTry (width - n - 1) ns []
            f x = x + n + 1
nextTry width (n:ns) (t:ts)
  | t >= width - n     = []
  | null ns && null ts = [t + 1]
  | rest0 /= []        = t : rest0
  | rest1 /= []        = t + 1 : rest1
  | otherwise          = []
      where rest0 = nextTry width ns ts
            rest1 = map f $ nextTry (width - n - t - 2) ns []
            f x = x + n + t + 2

valid :: [[Int]] -> [[Int]] -> [[Int]] -> Bool
valid rows cols ns = valid' rows cols ns (replicate (length cols) 0)

valid' :: [[Int]] -> [[Int]] -> [[Int]] -> [Int] -> Bool
valid' rows cols [] _ = if length rows >= maxCol then True else False
  where maxCol = maximum $ map (\c -> sum c + length c - 1) cols
valid' (r:rs) cols (n:ns) prevRow = if validRow then valid' rs newCols ns row else False
  where row = makeBinaryRow (length cols) r n
        (newCols, validRow) = checkRow row (zip cols (map (==1) prevRow))

checkRow :: [Int] -> [([Int], Bool)] -> ([[Int]], Bool)
checkRow [] [] = ([], True)
checkRow (x:xs) ((c, seq):cs)
  | length c == 0 = if x == 0 then (c : rest, restValid) else ([], False)
  | head c == 0   = if x == 0 then (tail c : rest, restValid) else ([], False)
  | seq           = if x == 1 then (((head c - 1):(tail c)) : rest, restValid) else ([], False)
  | x == 0        = (c : rest, restValid)
  | otherwise     = (((head c - 1):(tail c)) : rest, restValid)
      where (rest, restValid) = checkRow xs cs

makeBinaryRow :: Int -> [Int] -> [Int] -> [Int]
makeBinaryRow width [] [] = replicate width 0
makeBinaryRow width (k:ks) (n:ns) = replicate n 0 ++ replicate k 1 ++ rest
  where rest = makeBinaryRow (width - n - k) ks (map f ns)
        f x = x - n - k

solutionToStr :: [[Int]] -> [[Int]] -> [[Int]] -> String
solutionToStr (r:rs) cols (n:ns) = makeLine (length cols) r n ++ rowToStr r ++ "\n" ++ solutionToStr rs cols ns
  where rowToStr = concatMap (\k -> ' ' : show k)
solutionToStr [] cols []
  | any ((>0) . length) cols = concatMap headToStr cols ++ "\n" ++ solutionToStr [] (map getRest cols) []
  | otherwise                = ""
      where headToStr c = if length c > 0 then ' ' : show (head c) else "  "
            getRest c   = if length c > 0 then tail c else c
solutionToStr _ _ [] = "no solution"

makeLine :: Int -> [Int] -> [Int] -> String
makeLine width [] [] = (nstr width "|_") ++ "|"
makeLine width (k:ks) (n:ns) = (nstr n "|_") ++ (nstr k "|X") ++ makeLine (width - n - k) ks (map f ns)
  where f x = x - n - k

nstr :: Int -> String -> String
nstr n s = concat . (take n) $ repeat s
