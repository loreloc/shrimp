module Shrimp.Exception where

data Exception
  = -- | Define the empty program exception
    EmptyProgram
  | -- | Define an infinite loop exception
    InfiniteLoop
  | -- | Define a division by zero exception
    DivisionByZero
  | -- | Define an undeclared variable exception
    UndeclaredVariable String
  | -- | Define a multiple variable declaration exception
    MultipleVariable String
  | -- | Define a type mismatch exception
    TypeMismatch String
  | -- | Define an out of bound exception
    OutOfBound String Int 
  | -- | Define a non-positive array size definition
    InvalidSize String

instance Show Exception where
  show EmptyProgram = "Empty Program"
  show InfiniteLoop = "Infinite Loop"
  show DivisionByZero = "Division By Zero"
  show (UndeclaredVariable d) = "Undeclared Variable" ++ ": " ++ d
  show (MultipleVariable d) = "Multiple Variable" ++ ": " ++ d
  show (TypeMismatch d) = "Type Mismatch" ++ ": " ++ d
  show (OutOfBound d i) = "Out Of Bound" ++ ": " ++ d ++ " at " ++ show i
  show (InvalidSize d) = "Invalid Size" ++ ": " ++ d

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
