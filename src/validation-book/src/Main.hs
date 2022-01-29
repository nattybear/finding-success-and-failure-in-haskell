module Main where

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20) of
    True  -> Nothing
    False -> Just password

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (checkPasswordLength password)
