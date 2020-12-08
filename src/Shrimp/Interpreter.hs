module Shrimp.Interpreter where

import Control.Applicative
  ( liftA2,
  )
import Control.Monad
  ( join,
  )
import Shrimp.Exception
  ( Exception
      ( DivisionByZero,
        UndeclaredVariable
      ),
    Result (Error, Ok),
    exception,
  )
import Shrimp.Grammar
  ( ArithmeticExpr
      ( Add,
        Constant,
        Div,
        Identifier,
        Mod,
        Mul,
        Neg,
        Sub
      ),
    Block,
    BooleanExpr
      ( And,
        Boolean,
        Equal,
        Greater,
        GreaterEqual,
        Less,
        LessEqual,
        Not,
        NotEqual,
        Or
      ),
    Command
      ( Assignment,
        Branch,
        Loop,
        Skip
      ),
  )
import Shrimp.Optimizer
  ( optimize,
  )
import Shrimp.State
  ( State (..),
    empty,
    insert,
    search,
  )

-- | Safe division
safeDiv :: Int -> Int -> Result Int
safeDiv _ 0 = Error DivisionByZero
safeDiv u v = Ok (div u v)

-- | Safe modulus
safeMod :: Int -> Int -> Result Int
safeMod _ 0 = Error DivisionByZero
safeMod u v = Ok (mod u v)

-- | Evaluate an arithmetic expression given a state
evalArithmetic :: State -> ArithmeticExpr -> Result Int
evalArithmetic _ (Constant v) = Ok v
evalArithmetic s (Identifier d) =
  case search d s of
    Just v -> Ok v
    Nothing -> Error (UndeclaredVariable d)
evalArithmetic s (Add a1 a2) = liftA2 (+) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Sub a1 a2) = liftA2 (-) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Mul a1 a2) = liftA2 (*) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Div a1 a2) = join $ liftA2 safeDiv v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Mod a1 a2) = join $ liftA2 safeMod v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Neg a) = negate <$> v
  where
    v = evalArithmetic s a

-- | Evaluate a boolean expression given a state
evalBoolean :: State -> BooleanExpr -> Result Bool
evalBoolean _ (Boolean t) = Ok t
evalBoolean s (Not b) = not <$> evalBoolean s b
evalBoolean s (Or b1 b2) = liftA2 (||) t1 t2
  where
    t1 = evalBoolean s b1
    t2 = evalBoolean s b2
evalBoolean s (And b1 b2) = liftA2 (&&) t1 t2
  where
    t1 = evalBoolean s b1
    t2 = evalBoolean s b2
evalBoolean s (Equal a1 a2) = liftA2 (==) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (NotEqual a1 a2) = liftA2 (/=) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (Less a1 a2) = liftA2 (<) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (LessEqual a1 a2) = liftA2 (<=) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (Greater a1 a2) = liftA2 (>) v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalBoolean s (GreaterEqual a1 a2) = liftA2 (>=) v1 v2
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

-- | Run a program using an empty state as initial state
run :: Block -> State
run = execute empty . optimize
