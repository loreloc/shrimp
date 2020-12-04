module Shrimp.Exception where

import Control.Applicative
  ( Applicative (liftA2),
  )
import Control.Exception
  ( Exception,
  )
import Text.Printf
  ( printf,
  )

data RuntimeException
  = -- | Define an undeclared variable exception
    UndeclaredVariable String
  | -- | Define a multiple declaration exception
    MultipleDeclaration String

instance Exception RuntimeException

instance Show RuntimeException where
  show (UndeclaredVariable m) = printf "Undeclared variable '%s'" m
  show (MultipleDeclaration m) = printf "Multiple declaration '%s'" m

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
  liftA2 f (Ok u) (Ok v) = Ok (f u v)
  liftA2 _ (Error e) _ = Error e
  liftA2 _ _ (Error e) = Error e
