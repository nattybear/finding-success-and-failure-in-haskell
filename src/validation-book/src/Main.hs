module Main where

import Data.Char

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

checkLength :: Int -> String -> Either Error String
checkLength n xs =
  case (length xs > n) of
    True  -> Left (Error $ "Cannot be longer than "
                        ++ show n
                        ++ " characters.")
    False -> Right xs

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password = Password <$> (checkLength 20 password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name = Username <$> (checkLength 15 name)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left (Error "Cannot contain white space \
                         \or special characters.")
    True  -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Cannot be empty.")
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True  -> cleanWhitespace xs
    False -> Right (x : xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
    >>= requireAlphaNum
    >>= checkUsernameLength

main :: IO ()
main =
  putStrLn "Please enter a password\n> " >>
  (Password <$> getLine) >>=
  \pwd -> print (validatePassword pwd)
