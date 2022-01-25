validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  if null username
    then (if null password
            then "Empty username and password"
            else "Empty username")
    else (if null password
            then "Empty password"
            else "Okay")
