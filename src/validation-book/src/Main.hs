module Main where

import Data.Char

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (length password > 20) of
    True  -> Left "Your password cannot be longer \
                  \than 20 characters."
    False -> Right password

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

validatePassword :: String -> String
validatePassword password =
  case (cleanWhitespace password) of
    Nothing -> "Your password cannot be empty"
    Just password2 ->
      case (requireAlphaNum password2) of
        Nothing -> "Your password cannot contain \
                   \white space or special characters."
        Just password3 ->
          case (checkPasswordLength password3) of
            Nothing -> "Your password cannot be \
                       \longer than 20 characters."
            Just password4 -> password4

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)
