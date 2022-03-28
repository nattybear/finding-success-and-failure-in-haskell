import Data.Bool
import Data.Char
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

data These a b = This a | That b | These a b

instance LiftAB These where
  liftA = This
  liftB = That

instance MaybeAB These where
  maybeA (This   a   ) = Just a
  maybeA (That     _b) = Nothing
  maybeA (These  a _b) = Just a
  maybeB (This  _a   ) = Nothing
  maybeB (That      b) = Just b
  maybeB (These _a  b) = Just b

addTenIf x y = if (x > y) then (x + 10) else y

addTenBool x y = bool y (x + 10) (x > y)

foo :: String -> Int
foo s = case safeHead s of
  Nothing -> 0
  Just x  -> ord x

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' b _ Nothing = b
maybe' _ f (Just x) = f x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

class FoldAB f where
  foldAB :: (a -> c) -> (b -> c) -> f a b -> c

instance FoldAB Either where
  foldAB = either

instance FoldAB Validation where
  foldAB = validation