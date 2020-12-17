module Shrimp.Array where

-- | Array definition
type Array = [Int]

-- | Initialize a zero array
zeroArray :: Int -> Array
zeroArray n = replicate n 0

-- | Read an integer from the array
readArray :: Int -> Array -> Maybe Int
readArray i [] = Nothing
readArray i (v : vs)
  | i == 0 = Just v
  | i < 0 = Nothing
  | otherwise = readArray (i - 1) vs

-- | Write an integer into the array
writeArray :: Int -> Int -> Array -> Maybe Array
writeArray i v' []  = Nothing
writeArray i v' (v : vs)
  | i == 0 = Just (v' : vs)
  | i < 0 = Nothing
  | otherwise = case writeArray (i - 1) v' vs of
      Just vs' -> Just (v : vs')
      Nothing -> Nothing
