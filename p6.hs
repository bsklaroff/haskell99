main = putStrLn . show $ isPalindrome "madamimadam"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
