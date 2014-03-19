main = putStrLn $ table (\a b -> (a `and'` b `and'` a `or'` b))

table :: (Bool -> Bool -> Bool) -> [Char]
table f = "True True " ++ show (f True True) ++ "\n" ++
  "True False " ++ show (f True False) ++ "\n" ++
  "False True " ++ show (f False True) ++ "\n" ++
  "False False " ++ show (f False False) ++ "\n"

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nor' :: Bool -> Bool -> Bool
nor' = (not .) . or'

xor' :: Bool -> Bool -> Bool
xor' a b = and' (or' a b) (not (and' a b))

impl' :: Bool -> Bool -> Bool
impl' a b = or' (not a) b

equ' :: Bool -> Bool -> Bool
equ' a b = or' (and' a b) (nor' a b)

infixl 8 `and'`
infixl 7 `or'`
