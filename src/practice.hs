{-# OPTIONS_GHC -fdefer-typed-holes #-}

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
