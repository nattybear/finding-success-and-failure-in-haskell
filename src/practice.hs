{-# OPTIONS_GHC -fdefer-typed-holes #-}

import Data.Char
import Data.List

function :: Integer -> Integer -> Integer
function x y = if (x > y) then (x + 10) else y

function2 :: Integer -> Integer-> Integer
function2 x y =
  case (x > y) of
    False -> y       -- when False, return y
    True  -> x + 10  -- when True, return x + 10

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    True  -> Nothing
    False ->
      case (all isAlpha word) of
        False -> Nothing
        True  -> Just word

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
  case (isWord word1) of
    Nothing    -> "The first word is invalid."
    Just word1 ->
      case (isWord word2) of
        Nothing    -> "The second word is invalid."
        Just word2 -> _
