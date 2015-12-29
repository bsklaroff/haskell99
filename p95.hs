main = putStrLn . show $ fullWords 175

fullWords :: Int -> String
fullWords num = tail $ concatMap toWord numStr
  where numStr = show num
        toWord digit
          | digit == '0' = "-zero"
          | digit == '1' = "-one"
          | digit == '2' = "-two"
          | digit == '3' = "-three"
          | digit == '4' = "-four"
          | digit == '5' = "-five"
          | digit == '6' = "-six"
          | digit == '7' = "-seven"
          | digit == '8' = "-eight"
          | digit == '9' = "-nine"
