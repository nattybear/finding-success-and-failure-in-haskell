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
