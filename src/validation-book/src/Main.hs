module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password > 20) of
    True  -> Nothing
    False -> Just password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True  -> Just xs

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (checkPasswordLength password)
