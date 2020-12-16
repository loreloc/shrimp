module Shrimp.State where

import Shrimp.Array
  ( Array
  )

data Value
  = -- | Integer value
    IntegerValue Int
  | -- | Boolean value
    BooleanValue Bool 
  | -- | Array value
    ArrayValue Array

-- | The state (memory) of the interpreter
newtype State = State [(String, Value)]

instance Show Value where
  show (IntegerValue v) = "int" ++ " = " ++ show v
  show (BooleanValue t) = "bool" ++ " = " ++ show t
  show (ArrayValue vs) = "array" ++ "[" ++ show (length vs) ++ "]" ++ " = " ++ show vs

instance Show State where
  show (State []) = ""
  show (State ((d, v) : ss)) = s ++ "\n" ++ show (State ss)
    where s = "\t" ++ show d ++ ": " ++ show v

-- | Create an empty state
empty :: State
empty = State []

-- | Search of a variable in a state
search :: String -> State -> Maybe Value
search _ (State []) = Nothing
search d (State ((d', v) : ss)) =
  if d == d' then Just v else search d (State ss)

-- | Insert of a variable in a state
insert :: String -> Value -> State -> State
insert d v (State []) = State [(d, v)]
insert d v (State ((d', v') : ss)) =
  if d == d' then State ((d', v) : ss) else State ((d', v') : ss')
  where
    (State ss') = insert d v (State ss)
