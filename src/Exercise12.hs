data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue
  :: StringOrValue a
  -> (a -> StringOrValue b)
  -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val x) f = f x
