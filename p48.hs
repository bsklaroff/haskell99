main = putStrLn $ tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

tablen :: Int -> ([Bool] -> Bool) -> [Char]
tablen n f = foldr mf "" $ foldr ff [[]] (take n $ repeat [True, False])
  where ff x ps = [a : bs | a <- x, bs <- ps]
        mf xs ps = concatMap (\x -> show x ++ " ") xs ++ show (f xs) ++ "\n" ++ ps

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
