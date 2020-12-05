module Shrimp.Context where

import Shrimp.State
  ( State (..),
  )
import qualified Shrimp.State as State

-- | Define the context stack
newtype Context = Context [State]

-- | Create an empty context
empty :: Context
empty = Context [State.empty]

-- | Push an empty state in the context stack
push :: Context -> Context
push (Context ss) = Context (State [] : ss)

-- | Pop the head state in the context stack
pop :: Context -> Context
pop (Context ss) = Context (tail ss)

-- | Search in the context stack
search :: String -> Context -> Maybe Int
search _ (Context []) = Nothing
search d (Context (s : ss)) =
  case State.search d s of
    u@(Just _) -> u
    Nothing -> search d (Context ss)

-- | Insert of a variable in a context stack
insert :: String -> Int -> Context -> Context
insert d v (Context ss) = Context (s' : ss)
  where
    s' = State.insert d v (head ss)
