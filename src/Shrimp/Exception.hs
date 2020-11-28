module Shrimp.Exception where

import Text.Printf
import Control.Exception

data RuntimeException
    -- |Define an undeclared variable exception
    = UndeclaredVariable String
    -- |Define a multiple declaration exception
    | MultipleDeclaration String

instance Exception RuntimeException

instance Show RuntimeException where
    show (UndeclaredVariable m) = printf "Undeclared variable '%s'" m
    show (MultipleDeclaration m) = printf "Multiple declaration '%s'" m
