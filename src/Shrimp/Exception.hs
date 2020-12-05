module Shrimp.Exception where

data RuntimeException
  = -- | Define an undeclared variable exception
    UndeclaredVariable String
  deriving (Show)

-- | Define a result data structure
data Result a = Ok a | Error RuntimeException

instance Functor Result where
  fmap f (Ok v) = Ok (f v)
  fmap _ (Error e) = Error e

instance Applicative Result where
  pure v = Ok v
  (<*>) (Ok f) (Ok v) = Ok (f v)
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e

-- | Define a custom exception error
exception :: RuntimeException -> a
exception e = errorWithoutStackTrace $ show e
