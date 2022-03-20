import Data.Validation

class LiftAB f where
  liftA :: a -> f a b
  liftB :: b -> f a b

instance LiftAB Validation where
  liftA = Failure
  liftB = Success

instance LiftAB Either where
  liftA = Left
  liftB = Right

class MaybeAB f where
  maybeA :: f a b -> Maybe a
  maybeB :: f a b -> Maybe b

instance MaybeAB Validation where
  maybeA (Failure  a) = Just a
  maybeA (Success _b) = Nothing
  maybeB (Failure _a) = Nothing
  maybeB (Success  b) = Just b

instance MaybeAB Either where
  maybeA (Left   a) = Just a
  maybeA (Right _b) = Nothing
  maybeB (Left  _a) = Nothing
  maybeB (Right  b) = Just b
