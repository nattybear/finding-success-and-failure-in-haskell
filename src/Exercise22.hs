pureMaybe :: a -> Maybe a
pureMaybe = Just

pureEither :: a -> Either l a
pureEither = Right
