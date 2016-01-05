import Data.Char
import Data.Maybe
import Data.List as List
import Data.Array as Array
import Data.Set as Set

--main = putStrLn $ sudoku ".  .  4 | 8  .  . | .  1  7 6  7  . | 9  .  . | .  .  .  5  .  8 | .  3  . | .  .  4 --------+---------+-------- 3  .  . | 7  4  . | 1  .  .  .  6  9 | .  .  . | 7  8  .  .  .  1 | .  6  9 | .  .  5 --------+---------+-------- 1  .  . | .  8  . | 3  .  6 .  .  . | .  .  6 | .  9  1 2  4  . | .  .  1 | 5  .  ."
--main = putStrLn $ sudoku "0 5 0 0 6 0 0 0 1 0 0 4 8 0 0 0 7 0 8 0 0 0 0 0 0 5 2 2 0 0 0 5 7 0 3 0 0 0 0 0 0 0 0 0 0 0 3 0 6 9 0 0 0 5 7 9 0 0 0 0 0 0 8 0 1 0 0 0 6 5 0 0 5 0 0 0 3 0 0 6 0"
main = putStrLn $ sudoku "0 0 0 0 6 0 0 8 0 0 2 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 7 0 0 0 0 1 0 2 5 0 0 0 3 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 4 2 0 1 0 0 0 3 0 0 7 0 0 6 0 0 0 0 0 0 0 0 0 5 0"

sudoku :: String -> String
sudoku board = solutionToStr boardList . solveBoard $ boardList
  where boardList = parseBoard board

parseBoard :: String -> [Int]
parseBoard "" = []
parseBoard (n:ns)
  | n == '.'            = 0 : parseBoard ns
  | n `elem` ['0'..'9'] = digitToInt n : parseBoard ns
  | otherwise           = parseBoard ns

solveBoard :: [Int] -> [Int]
solveBoard singles
  | initValid && done initVals = List.map (head . toList . fst) . Array.elems $ initVals
  | initValid && finalValid    = List.map (head . toList . fst) . Array.elems $ finalVals
  | otherwise                  = []
      where emptyVals = listArray (0, 80) . take 81 . repeat $ (fromList [1..9], False)
            (initVals, initValid) = iterElim emptyVals singles
            (finalVals, finalValid) = guessAndCheck initVals

guessAndCheck :: Array Int (Set Int, Bool) -> (Array Int (Set Int, Bool), Bool)
guessAndCheck vals
  | nextValid && done nextVals = (nextVals, True)
  | nextValid && branchValid   = (branchVals, True)
  | lastGuess                  = (vals, False)
  | otherwise                  = guessAndCheck (vals // [elimVal vals guess pos])
      where (guess, pos) = findGuess vals
            singles = take pos (repeat 0) ++ guess : take (81 - pos - 1) (repeat 0)
            (nextVals, nextValid) = iterElim vals singles
            (branchVals, branchValid) = guessAndCheck nextVals
            lastGuess = (==1) . size . fst $ vals ! pos

done :: Array Int (Set Int, Bool) -> Bool
done vals = all (==1) . List.map (size . fst) $ Array.elems vals

findGuess :: Array Int (Set Int, Bool) -> (Int, Int)
findGuess vals = (head . toList . fst $ guesses, pos)
  where valList = Array.elems vals
        optsLens = List.map (\(opts, seen) -> if seen then 11 else length opts) valList
        pos = fromJust $ (minimum optsLens) `elemIndex` optsLens
        guesses = vals ! pos

iterElim :: Array Int (Set Int, Bool) -> [Int] -> (Array Int (Set Int, Bool), Bool)
iterElim vals singles = if anyNewSingles && nextValid then iterElim newVals newSingles else (newVals, nextValid)
  where newVals = elimArr vals singles
        newValList = Array.elems newVals
        newSingles = List.map selectNewSingles newValList
        selectNewSingles (opts, seen)
          | seen == True || size opts /= 1 = 0
          | otherwise                      = head $ toList opts
        anyNewSingles = any (/=0) newSingles
        nextValid = all (>0) $ List.map (size . fst) newValList

elimArr :: Array Int (Set Int, Bool) -> [Int] -> Array Int (Set Int, Bool)
elimArr vals arr = fst $ List.foldr (\x (newVals, pos) -> (updateVal newVals pos x, pos + 1)) (vals, 0) (reverse arr)

updateVal :: Array Int (Set Int, Bool) -> Int -> Int -> Array Int (Set Int, Bool)
updateVal vals pos x
  | x == 0    = vals
  | otherwise = updatedVals
      where updatedHoriz = elim vals (getRow pos) x
            updatedVert = elim updatedHoriz (getCol pos) x
            updatedBox = elim updatedVert (getBox pos) x
            updatedVals = updatedBox // [(pos, (singleton x, True))]

elim :: Array Int (Set Int, Bool) -> [Int] -> Int -> Array Int (Set Int, Bool)
elim vals targets x = vals // (List.map (elimVal vals x) targets)

elimVal :: Array Int (Set Int, Bool) -> Int -> Int -> (Int, (Set Int, Bool))
elimVal vals x pos = (pos, (Set.delete x . fst $ vals!pos, snd $ vals!pos))

getRow :: Int -> [Int]
getRow x = [floorX..floorX + 8]
  where floorX = (x `div` 9) * 9

getCol :: Int -> [Int]
getCol x = take 9 [modX, modX + 9..]
  where modX = x `mod` 9

getBox :: Int -> [Int]
getBox x = [fx, fx + 1, fx + 2, fx + 9, fx + 10, fx + 11, fx + 18, fx + 19, fx + 20]
  where fx = (x `div` 27) * 27 + ((x `mod` 9) `div` 3) * 3

solutionToStr :: [Int] -> [Int] -> String
solutionToStr inList outList = solutionToStr' inList outList 0

solutionToStr' :: [Int] -> [Int] -> Int -> String
solutionToStr' ins outs row
  | row == 12                            = ""
  | row == 0 && length outs == 0         = "problem" ++ titleSpaces ++ "no solution\n" ++ parseSame
  | row == 0                             = "problem" ++ titleSpaces ++ "solution\n" ++ parseSame
  | row `mod` 4 == 0 && length outs == 0 = dashesPlus ++ spaces ++ "\n" ++ parseSame
  | row `mod` 4 == 0                     = dashesPlus ++ spaces ++ dashesPlus ++ "\n" ++ parseSame
  | otherwise                            = (nums ins) ++ spaces ++ (nums outs) ++ "\n" ++ parseRest
      where parseSame = solutionToStr' ins outs (row + 1)
            parseRest = solutionToStr' (drop 9 ins) (drop 9 outs) (row + 1)
            titleSpaces = take 9 $ repeat ' '
            spaces = take 5 $ repeat ' '
            dashesPlus = "---+---+---"
            nums [] = ""
            nums ns = n0 ns ++ "|" ++ n1 ns ++ "|" ++ n2 ns
            n0 ns = List.map printSquare (take 3 ns)
            n1 ns = List.map printSquare (take 3 $ drop 3 ns)
            n2 ns = List.map printSquare (take 3 $ drop 6 ns)
            printSquare n = if n == 0 then '.' else intToDigit n
