import Data.List
import qualified Data.Map as Map

-- Assumes input board is fully connected
main = putStr $ crossword "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n"

crossword :: String -> String
crossword = solutionToStr . solve . readCrossword

readCrossword :: String -> ([String], [(Int, Int, Bool, Int)])
readCrossword str = (words, parseBoard strBoard)
  where strLines = lines str
        (words, strBoard) = parseWords strLines

parseWords :: [String] -> ([String], [String])
parseWords (n:ns)
  | n == ""   = ([], ns)
  | otherwise = (n:restWords, strBoard)
      where (restWords, strBoard) = parseWords ns

parseBoard :: [String] -> [(Int, Int, Bool, Int)]
parseBoard ns = parseBoard' ns (replicate width 0) 0
  where width = maximum $ map length ns

parseBoard' :: [String] -> [Int] -> Int -> [(Int, Int, Bool, Int)]
parseBoard' [] cols row = makeVerts cols row
parseBoard' (n:ns) cols row
  | n == ""   = makeVerts cols row
  | otherwise = seqs ++ parseBoard' ns newCols (row + 1)
      where (seqs, newCols) = parseRow (fillRow n (length cols)) cols row (length cols) 0
            fillRow str len = take len (str ++ repeat ' ')

makeVerts :: [Int] -> Int -> [(Int, Int, Bool, Int)]
makeVerts ns row = makeVerts' ns row 0

makeVerts' :: [Int] -> Int -> Int -> [(Int, Int, Bool, Int)]
makeVerts' [] _ _ = []
makeVerts' (n:ns) row col
  | n <= 1    = rest
  | otherwise = makeVert n row col : rest
      where rest = makeVerts' ns row (col + 1)

parseRow :: String -> [Int] -> Int -> Int -> Int -> ([(Int, Int, Bool, Int)], [Int])
parseRow [] [] row width curLen = if curLen <= 1 then ([], []) else ([makeHoriz curLen row width], [])
parseRow (n:ns) (c:cs) row width curLen
  | n == ' ' = (endRow ++ endCol ++ spaceRest, 0 : spaceCols)
  | n == '.' = (dotRest, (c + 1) : dotCols)
      where (spaceRest, spaceCols) = parseRow ns cs row width 0
            (dotRest, dotCols) = parseRow ns cs row width (curLen + 1)
            endRow = if curLen <= 1 then [] else [makeHoriz curLen row (width - (length ns) - 1)]
            endCol = if c <= 1 then [] else [makeVert c row (width - (length ns) - 1)]

makeVert :: Int -> Int -> Int -> (Int, Int, Bool, Int)
makeVert len row col = (row - len, col, False, len)

makeHoriz :: Int -> Int -> Int -> (Int, Int, Bool, Int)
makeHoriz len row col = (row, col - len, True, len)

solve :: ([String], [(Int, Int, Bool, Int)]) -> Map.Map (Int, Int, Bool, Int) String
solve (words, board) = solve' (lenMap words) boardCollisions collisions [head board] 0 Map.empty
  where collisions = findCollisions board Map.empty
        boardCollisions = foldr (\s acc -> Map.insert s (seqCollisions s) acc) Map.empty board
        seqCollisions = filter (\x -> Map.member x collisions) . spaces

solve' :: Map.Map Int [String] -> Map.Map (Int, Int, Bool, Int) [(Int, Int)] -> Map.Map (Int, Int) [(Int, Int, Bool, Int)] -> [(Int, Int, Bool, Int)] -> Int -> Map.Map (Int, Int, Bool, Int) String -> Map.Map (Int, Int, Bool, Int) String
solve' wordsMap _ _ [] _ res | null wordsMap = res
solve' wordsMap boardCollisions collisions (seq@(row, col, horiz, len):ns) wordIter res
  | Map.member seq res              = solve' wordsMap boardCollisions collisions ns 0 res
  | wordIter >= length possWords    = Map.empty
  | validWord && not (null thisTry) = thisTry
  | not (null nextTry)              = nextTry
  | otherwise                       = Map.empty
      where possWords = (wordsMap Map.! len)
            nextWord = possWords !! wordIter
            nextRes = Map.insert seq nextWord res
            validWord = valid boardCollisions collisions nextRes seq
            nextSeqs = map (\s -> head (filter (/=seq) (collisions Map.! s))) (boardCollisions Map.! seq)
            thisTry = solve' (removeWord nextWord wordsMap) boardCollisions collisions (ns ++ nextSeqs) 0 nextRes
            nextTry = solve' wordsMap boardCollisions collisions (seq:ns) (wordIter + 1) res

removeWord :: String -> Map.Map Int [String] -> Map.Map Int [String]
removeWord word wordsMap
  | length nextWords == 0 = Map.delete k wordsMap
  | otherwise             = Map.insert k nextWords wordsMap
      where k = length word
            possWords = wordsMap Map.! k
            nextWords = [x | x <- possWords, x /= word]

valid :: Map.Map (Int, Int, Bool, Int) [(Int, Int)] -> Map.Map (Int, Int) [(Int, Int, Bool, Int)] -> Map.Map (Int, Int, Bool, Int) String -> (Int, Int, Bool, Int) -> Bool
valid boardCollisions collisions res seq = all (==True) (map (\x -> collisionOkay x res (collisions Map.! x)) (boardCollisions Map.! seq))

collisionOkay :: (Int, Int) -> Map.Map (Int, Int, Bool, Int) String -> [(Int, Int, Bool, Int)] -> Bool
collisionOkay space res ns = (==1) . length . nub . filter (/='*') $ map getLetter ns
  where getDist (row, col, horiz, _) (sr, sc) = if horiz then sc - col else sr - row
        getLetter n
          | Map.member n res = (res Map.! n) !! (getDist n space)
          | otherwise        = '*'

lenMap :: [String] -> Map.Map Int [String]
lenMap words = lenMap' words Map.empty
  where lenMap' [] m = m
        lenMap' (w:ws) m
          | Map.member wLen m = lenMap' ws (Map.insert wLen (w : (m Map.! wLen)) m)
          | otherwise         = lenMap' ws (Map.insert wLen [w] m)
              where wLen = length w

findCollisions :: [(Int, Int, Bool, Int)] -> Map.Map (Int, Int) [(Int, Int, Bool, Int)] -> Map.Map (Int, Int) [(Int, Int, Bool, Int)]
findCollisions [] m = Map.filter ((>1) . length) m
findCollisions (n:ns) m = findCollisions ns (foldr (upsert n) m (spaces n))
  where upsert v k acc
          | Map.member k acc = Map.insert k (v : (acc Map.! k)) acc
          | otherwise        = Map.insert k [v] acc

spaces :: (Int, Int, Bool, Int) -> [(Int, Int)]
spaces (_, _, _, 0) = []
spaces (row, col, True, k) = (row, col) : spaces (row, col + 1, True, k - 1)
spaces (row, col, False, k) = (row, col) : spaces (row + 1, col, False, k - 1)

numCollisions :: Map.Map (Int, Int) [(Int, Int, Bool, Int)] -> (Int, Int, Bool, Int) -> Int
numCollisions m = length . filter (\x -> Map.member x m) . spaces

solutionToStr :: Map.Map (Int, Int, Bool, Int) String -> String
solutionToStr m
  | null m    = "no solution\n"
  | otherwise = solutionToStr' (getAllLetters m) 0

solutionToStr' :: [(Int, Int, Char)] -> Int -> String
solutionToStr' [] _ = []
solutionToStr' letters row = makeRow (takeWhile (inRow row) letters) 0 ++ solutionToStr' (dropWhile (inRow row) letters) (row + 1)
  where inRow r (letterRow, _, _) = r == letterRow

makeRow :: [(Int, Int, Char)] -> Int -> String
makeRow [] _ = "\n"
makeRow letters@((_, letterCol, c):rest) col
  | col == letterCol = c : makeRow rest (col + 1)
  | otherwise        = ' ' : makeRow letters (col + 1)

getAllLetters :: Map.Map (Int, Int, Bool, Int) String -> [(Int, Int, Char)]
getAllLetters = sort . nub . concat . Map.elems . Map.mapWithKey getSeqLetters

getSeqLetters :: (Int, Int, Bool, Int) -> String -> [(Int, Int, Char)]
getSeqLetters (row, col, horiz, 0) [] = []
getSeqLetters (row, col, horiz, len) (c:rest) = (row, col, c) : getSeqLetters (nextRow, nextCol, horiz, len - 1) rest
  where nextRow = if horiz then row else row + 1
        nextCol = if horiz then col + 1 else col
