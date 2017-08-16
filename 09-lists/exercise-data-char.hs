import Data.Char (isUpper, toUpper)

filterUpper :: String -> String
filterUpper s = filter isUpper s

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = (toUpper x) : xs

uppercase :: String -> String
uppercase ""     = ""
uppercase (x:xs) = (toUpper x) : uppercase xs
