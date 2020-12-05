module Shrimp.Interpreter where

import Shrimp.Exception
  ( Result (Error, Ok),
    RuntimeException (UndeclaredVariable),
    exception,
  )
import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul, Sub),
    Block,
    BooleanExpr (And, Boolean, Equal, LessEqual, Not, Or),
    Command (Assignment, Branch, Loop, Skip),
  )
import Shrimp.State
  ( State (..),
    insert,
    search,
  )

-- | Evaluate an arithmetic expression given a state
evalArithmetic :: State -> ArithmeticExpr -> Result Int
evalArithmetic _ (Constant v) = Ok v
evalArithmetic s (Identifier d) =
  case search d s of
    Just v -> Ok v
    Nothing -> Error (UndeclaredVariable d)
evalArithmetic s (Add a1 a2) = (+) <$> v1 <*> v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Sub a1 a2) = (-) <$> v1 <*> v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Mul a1 a2) = (*) <$> v1 <*> v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2

-- | Evaluate a boolean expression given a state
evalBoolean :: State -> BooleanExpr -> Result Bool
evalBoolean _ (Boolean t) = Ok t
evalBoolean s (Not b) = not <$> evalBoolean s b
evalBoolean s (Or b1 b2) = (||) <$> t1 <*> t2
  where
    t1 = evalBoolean s b1
    t2 = evalBoolean s b2
evalBoolean s (And b1 b2) = (&&) <$> t1 <*> t2
  where
    t1 = evalBoolean s b1
    t2 = evalBoolean s b2
evalBoolean s (Equal a1 a2) = (==) <$> v1 <*> v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (LessEqual a1 a2) = (<=) <$> v1 <*> v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2

-- | Execute a program given a state
execute :: State -> Block -> State
execute s [] = s
execute s (Skip : cs) = execute s cs
execute s ((Assignment d a) : cs) =
  case evalArithmetic s a of
    Ok v -> execute s' cs
      where
        s' = insert d v s
    Error e -> exception e
execute s ((Branch b cs' cs'') : cs) =
  case evalBoolean s b of
    Ok True -> execute s (cs' ++ cs)
    Ok False -> execute s (cs'' ++ cs)
    Error e -> exception e
execute s (c@(Loop b cs') : cs) =
  execute s (Branch b (cs' ++ [c]) [Skip] : cs)
