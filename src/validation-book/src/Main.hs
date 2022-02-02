module Main where

import Data.Char

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (length password > 20) of
    True  -> Left "Your password cannot be longer \
                  \than 20 characters."
    False -> Right password

requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True  -> Just xs

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Nothing
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True  -> cleanWhitespace xs
    False -> Just (x : xs)

validatePassword :: String -> Either String String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)
