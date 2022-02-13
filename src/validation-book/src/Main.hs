module Main where

import Data.Char

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case (length password > 20) of
    True  -> Left (Error "Your password cannot be longer \
                         \than 20 characters.")
    False -> Right (Password password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name =
  case (length name > 15) of
    True  -> Left (Error "Username cannot be longer \
                         \than 15 characters.")
    False -> Right (Username name)

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
    >>= checkPasswordLength
    >>= requireAlphaNum

main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)

printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True  -> Right ()
    False -> Left (unlines
      [ "Test " ++ show n
      , "  Expected:  " ++ show expected
      , "  But got:   " ++ show actual
      ])

test :: IO ()
test = printTestResult $ do
  eq 1 (checkPasswordLength "") (Right "")
  eq 2 (checkPasswordLength "julielovesbooks")
       (Right "julielovesbooks")
