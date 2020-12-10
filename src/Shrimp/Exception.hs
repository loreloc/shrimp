module Shrimp.Exception where

data Exception
  = -- | Define an infinite loop exception
    InfiniteLoop
  | -- | Define a division by zero exception
    DivisionByZero
  | -- | Define an undeclared variable exception
    UndeclaredVariable String

instance Show Exception where
  show InfiniteLoop = "Infinite Loop"
  show DivisionByZero = "Division By Zero"
  show (UndeclaredVariable d) = "Undeclared Variable" ++ ": " ++ d

-- | Define a result data structure
data Result a = Ok a | Error Exception

instance Functor Result where
  fmap f (Ok v) = Ok (f v)
  fmap _ (Error e) = Error e

instance Applicative Result where
  pure v = Ok v
  (<*>) (Ok f) (Ok v) = Ok (f v)
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e

instance Monad Result where
  (>>=) (Ok v) f = f v
  (>>=) (Error e) _ = Error e

-- | Define a custom exception error
exception :: Exception -> a
exception e = errorWithoutStackTrace $ show e
