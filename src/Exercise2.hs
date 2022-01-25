validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  case (null username, null password) of
    (True,  True)  -> "Empty username and password"
    (True,  False) -> "Empty username"
    (False, True)  -> "Empty password"
    (False, False) -> "Okay"
