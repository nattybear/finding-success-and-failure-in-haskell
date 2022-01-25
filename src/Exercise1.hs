absVal :: (Num a, Ord a) => a -> a
absVal x = if (x < 0) then (negate x) else x
