{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Char
import Data.Coerce
import Data.Validation

newtype Password = Password String
  deriving Show

newtype Error = Error [String]
  deriving (Semigroup, Show)

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show

newtype UserPW = UserPW Password

newtype AdminPW = AdminPW Password

type Rule a = (a -> Validation Error a)

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case (length password > 20) of
    True  -> Failure (Error ["Your password cannot be longer \
                             \than 20 characters."])
    False -> Success (Password password)

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name =
  case (length name > 15) of
    True  -> Failure (Error ["Username cannot be longer \
                             \than 15 characters."])
    False -> Success (Username name)

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (Error ["Cannot contain white space \
                             \or special characters."])
    True  -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Cannot be empty."])
cleanWhitespace (x : xs) =
  case (isSpace x) of
    True  -> cleanWhitespace xs
    False -> Success (x : xs)

validatePassword :: Rule Password
validatePassword password =
  case (coerce cleanWhitespace :: Rule Password) password of
    Failure err -> Failure err
    Success password2 -> (coerce requireAlphaNum :: Rule Password) password2 *>
                         (coerce checkPasswordLength :: Rule Password) password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case (cleanWhitespace username) of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *>
                         checkUsernameLength username2

passwordErrors :: Password -> Validation Error Password
passwordErrors password =
  case validatePassword password of
    Failure err -> Failure (Error ["Invalid password:"]
                            <> err)
    Success password2 -> Success password2

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case validateUsername username of
    Failure err -> Failure (Error ["Invalid username:"]
                            <> err)
    Success username2 -> Success username2

makeUser :: Username -> Password -> Validation Error User
makeUser name password =
  User <$> usernameErrors name
       <*> passwordErrors password

display :: Username -> Password -> IO ()
display name password =
  case makeUser name password of
    Failure err -> putStr (unlines (coerce err))
    Success (User name password) ->
      putStrLn ("Welcome, " ++ coerce name)

errorCoerce :: Error -> [String]
errorCoerce (Error err) = err

passwordCoerce :: Password -> String
passwordCoerce (Password x) = x

usernameCoerce :: Username -> String
usernameCoerce (Username x) = x

userPasswordCoerce :: UserPW -> Password
userPasswordCoerce (UserPW pw) = pw

userPasswordCoerce' :: UserPW -> String
userPasswordCoerce' = coerce

userPasswordCoerceBack :: String -> UserPW
userPasswordCoerceBack = UserPW . Password

adminPasswordCoerce :: AdminPW -> Password
adminPasswordCoerce (AdminPW pw) = pw


main :: IO ()
main = do
  putStr "Please enter a username\n> "
  username <- Username <$> getLine
  putStr "Please enter a password\n> "
  password <- Password <$> getLine
  display username password
