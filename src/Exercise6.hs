substituteChar :: Char -> Char
substituteChar c =
  case c of
    'b' -> '8'
    'd' -> '>'
    'e' -> '3'
    'f' -> '7'
    'g' -> '9'
    'h' -> '4'
    'i' -> '1'
    'j' -> '7'
    'o' -> '0'
    'q' -> '9'
    'x' -> '%'
    'z' -> '5'
    _   -> c

translateWord :: String -> String
translateWord = map substituteChar

main :: IO ()
main = do
  putStr "Please enter a word.\n> "
  word <- getLine
  putStrLn $ translateWord word
