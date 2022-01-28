{-# OPTIONS_GHC -fdefer-typed-holes #-}

substituteChar :: Char -> Char
substituteChar c =
  case c of
    'e' -> '3'
    _   -> c

translateWord :: String -> String
translateWord word = _
