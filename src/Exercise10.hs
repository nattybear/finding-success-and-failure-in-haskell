reverseLine :: IO ()
reverseLine = getLine >>= (print . reverse)
