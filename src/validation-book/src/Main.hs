{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Validation
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Password = Password T.Text
  deriving Show

newtype Error = Error [T.Text]
  deriving (Semigroup, Show)

newtype Username = Username T.Text
  deriving Show

data User = User Username Password
  deriving Show

checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength password =
  case (T.length password > 20) of
    True  -> Failure (Error ["Your password cannot be longer \
                             \than 20 characters."])
    False -> Success (Password password)

checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength name =
  case (T.length name > 15) of
    True  -> Failure (Error ["Username cannot be longer \
                             \than 15 characters."])
    False -> Success (Username name)

requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum xs =
  case (T.all isAlphaNum xs) of
    False -> Failure (Error ["Cannot contain white space \
                             \or special characters."])
    True  -> Success xs

cleanWhitespace :: T.Text -> Validation Error T.Text
cleanWhitespace t = if T.null t
                    then Failure (Error ["Cannot be empty."])
                    else Success $ T.strip t

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case (cleanWhitespace password) of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *>
                         checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case (cleanWhitespace username) of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *>
                         checkUsernameLength username2

makeUser :: Username -> Password -> Validation Error User
makeUser name password =
  User <$> validateUsername name
       <*> validatePassword password

main :: IO ()
main = do
  putStr "Please enter a username\n> "
  username <- Username <$> T.getLine
  putStr "Please enter a password\n> "
  password <- Password <$> T.getLine
  print (makeUser username password)
