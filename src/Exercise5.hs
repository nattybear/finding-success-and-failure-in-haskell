import Data.Char

isPalindrome :: String -> Bool
isPalindrome word = word == reverse word

simplify :: String -> String
simplify word = map toUpper word'
  where word' = filter (\x -> not (isSpace x || isPunctuation x)) word

isWord :: String -> Maybe String
isWord word =
  case (null word) of
    True  -> Nothing
    False ->
      case (all isAlpha word') of
        False -> Nothing
        True  -> Just word'
  where word' = simplify word

checkPalindrome :: String -> String
checkPalindrome word =
  case (isWord word) of
    Nothing   -> "The word is invalid."
    Just word ->
      case (isPalindrome word) of
        False -> "The word is not panlidrome."
        True  -> "The word is palindrome."

main :: IO ()
main = do
  putStr "Please enter a word.\n> "
  word <- getLine
  print (checkPalindrome word)
