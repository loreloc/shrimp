module Shrimp.Array where

import Shrimp.Exception
  ( Result (Ok, Error),
    Exception (OutOfBound),
  )

-- | Array definition
type Array = [Int]

-- | Initialize a zero array
zeroArray :: Int -> Array
zeroArray n = replicate n 0

-- | Read an integer from the array
readArray :: Int -> Array -> Result Int
readArray i [] = Error OutOfBound
readArray i (v : vs)
  | i == 0 = Ok v
  | i < 0 = Error OutOfBound
  | otherwise = readArray (i - 1) vs

-- | Write an integer into the array
writeArray :: Int -> Int -> Array -> Result Array
writeArray i v' []  = Error OutOfBound
writeArray i v' (v : vs)
  | i == 0 = Ok (v' : vs)
  | i < 0 = Error OutOfBound
  | otherwise = writeArray (i - 1) v' vs
