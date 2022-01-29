module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (len > 20 || len < 10) of
    True  -> Nothing
    False -> Just password
  where len = length password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True  -> Just xs

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True  -> cleanWhitespace xs
    False -> Just (x : xs)

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (checkPasswordLength password)
