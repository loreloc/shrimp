module Shrimp.State where

-- | The state (memory) of the interpreter
newtype State = State [(String, Int)]

-- | Create an empty state
empty :: State
empty = State []

-- | Search of a variable in a state
search :: String -> State -> Maybe Int
search _ (State []) = Nothing
search d (State ((d', v) : ss)) =
  if d == d' then Just v else search d (State ss)

-- | Insert (or modification) of a variable in a state
insert :: String -> Int -> State -> State
insert d v (State []) = State [(d, v)]
insert d v (State ((d', v') : ss)) =
  if d == d' then State ((d', v) : ss) else State ((d', v') : ss')
  where
    (State ss') = insert d v (State ss)
