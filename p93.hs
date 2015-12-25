main = do mapM_ putStrLn $ puzzle [2,3,5,7,11]

puzzle :: [Rational] -> [String]
puzzle = map makeStr . filter correct . genEqs

genEqs :: [Rational] -> [(([Rational], String), ([Rational], String))]
genEqs ns = [((take k ns, before), (drop k ns, after)) | k <- [1..length ns - 1], before <- genArith (k - 1), after <- genArith (length ns - k - 1)]

genArith :: Int -> [String]
genArith = (map genArith' [0..] !!)
  where genArith' 0 = [""]
        genArith' n = concatMap genParens [(op, rest) | op <- "*/+-", rest <- genArith (n - 1)]

genParens :: (Char, String) -> [String]
genParens (op, rest) = (op:rest) : addParens op rest ++ addParens2 (op:rest)

addParens :: Char -> String -> [String]
addParens op rest
  | op == '-' || op == '*' = addParens' op rest False
  | op == '/'              = addParens' op rest True
  | otherwise              = []

addParens' :: Char -> String -> Bool -> [String]
addParens' op rest seenPM = map (\(r0, r1) -> op : '(' : r0 ++ ')' : r1) (splitRest rest "" 0 seenPM)

splitRest :: String -> String -> Int -> Bool -> [(String, String)]
splitRest "" _ _ False = []
splitRest "" "" _ _ = []
splitRest "" rest1 0 True = [(reverse rest1, "")]
splitRest ('(':rest0) rest1 k seenPM = splitRest rest0 ('(':rest1) (k + 1) seenPM
splitRest (op:'(':rest0) rest1 0 seenPM = splitRest rest0 ('(':op:rest1) 1 (seenPM || op == '+' || op == '-')
splitRest (op:'(':rest0) rest1 k seenPM = splitRest rest0 ('(':op:rest1) (k + 1) seenPM
splitRest (')':rest0) rest1 k seenPM = splitRest rest0 (')':rest1) (k - 1) seenPM
splitRest (op:rest0) rest1 k seenPM
  | k == 0 && (seenPM || op == '+' || op == '-') && (length rest0 == 0) = splitRest rest0 (op:rest1) 0 True
  | k == 0 && (seenPM || op == '+' || op == '-')                        = (reverse (op:rest1), rest0) : splitRest rest0 (op:rest1) 0 True
  | otherwise                                                           = splitRest rest0 (op:rest1) k seenPM

addParens2 :: String -> [String]
addParens2 ops = map (\(r0, r1) -> '(' : r0 ++ ')' : r1) (splitRest2 ops "" 0 False)

splitRest2 :: String -> String -> Int -> Bool -> [(String, String)]
splitRest2 "" _ _ _ = []
splitRest2 (op:'(':rest0) rest1 0 seenPM = splitRest2 rest0 ('(':op:rest1) 1 (seenPM || op == '+' || op == '-')
splitRest2 (op:'(':rest0) rest1 k seenPM = splitRest2 rest0 ('(':op:rest1) (k + 1) seenPM
splitRest2 (')':rest0) rest1 k seenPM = splitRest2 rest0 (')':rest1) (k - 1) seenPM
splitRest2 (op:rest0) rest1 k seenPM
  | k == 0 && seenPM && (op == '*' || op == '/') = (reverse rest1, (op:rest0)) : splitRest2 rest0 (op:rest1) 0 True
  | k == 0                                       = splitRest2 rest0 (op:rest1) 0 (seenPM || op == '+' || op == '-')
  | otherwise                                    = splitRest2 rest0 (op:rest1) k seenPM

correct :: (([Rational], String), ([Rational], String)) -> Bool
correct x = x0 == x1 && x0 /= ([], "")
  where (x0ns, x0ops) = fst x
        (x1ns, x1ops) = snd x
        x0 = compute x0ns x0ops
        x1 = compute x1ns x1ops

compute :: [Rational] -> String -> ([Rational], String)
compute ns ops = computeA nextNs nextOps
  where (nextNs, nextOps) = computeM ns ops

computeM :: [Rational] -> String -> ([Rational], String)
computeM ns "" = (ns, "")
computeM ns (')':ops) = (ns, ')':ops)
computeM ns ('(':ops) = (resNs, resOps)
  where (nextNs, nextOps) = compute ns ops
        (resNs, resOps) = computeM nextNs nextOps
computeM (n:ns) (op:'(':ops)
  | compute ns ops == ([], "") = ([], "")
  | op == '*'                  = computeM (n * nextN : nextNs) nextOps
  | op == '/'                  = if nextN /= 0 then computeM (n / nextN : nextNs) nextOps else ([], "")
  | otherwise                  = (n : resNs, op : resOps)
      where (nextN:nextNs, nextOps) = compute ns ops
            (resNs, resOps) = computeM (nextN:nextNs) nextOps
computeM (n0:n1:ns) (op:ops)
  | op == '*' = computeM (n0 * n1 : ns) ops
  | op == '/' = if n1 /= 0 then computeM (n0 / n1 : ns) ops else ([], "")
  | otherwise = (n0 : resNs, op : resOps)
      where (resNs, resOps) = computeM (n1:ns) ops

computeA :: [Rational] -> String -> ([Rational], String)
computeA ns "" = (ns, "")
computeA ns (')':ops) = (ns, ops)
computeA (n0:n1:ns) (op:ops)
  | op == '+' = computeA (n0 + n1 : ns) ops
  | op == '-' = computeA (n0 - n1 : ns) ops
computeA _ _ = ([], "")

makeStr :: (([Rational], String), ([Rational], String)) -> String
makeStr x = (makeStr' x0ns x0ops) ++ " = " ++ (makeStr' x1ns x1ops)
  where (x0ns, x0ops) = fst x
        (x1ns, x1ops) = snd x
        makeStr' [] ops = ops
        makeStr' (n:ns) "" = show (round n)
        makeStr' (n:ns) (')':ops) = show (round n) ++ ')' : parens ++ nextOp ++ makeStr' ns resOps
          where parens = takeWhile (==')') ops
                nextOps = dropWhile (==')') ops
                nextOp = take 1 nextOps
                resOps = drop 1 nextOps
        makeStr' ns ('(':ops) = '(' : makeStr' ns ops
        makeStr' (n:ns) (op:ops) = show (round n) ++ op : makeStr' ns ops
