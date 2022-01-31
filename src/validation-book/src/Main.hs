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

combineThemAll :: String -> Maybe String
combineThemAll xs =
  case cleanWhitespace xs of
    Nothing  -> Nothing
    Just xs' ->
      case requireAlphaNum xs' of
        Nothing  -> Nothing
        Just xs' ->
          case checkPasswordLength xs' of
            Nothing  -> Nothing
            Just xs' -> Just xs'

validatePassword :: String -> Maybe String
validatePassword password =
  case (cleanWhitespace password) of
    Nothing -> Nothing
    Just password2 ->
      requireAlphaNum password2

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (combineThemAll password)
