substituteChar :: Char -> Char
substituteChar c =
  case c of
    'e' -> '3'
    _   -> c

translateWord :: String -> String
translateWord = map substituteChar

main :: IO ()
main = do
  putStr "Please enter a word.\n> "
  word <- getLine
  putStrLn $ translateWord word
