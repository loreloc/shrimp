module Shrimp.Interpreter where

import Shrimp.Array (Array, readArray, writeArray, zeroArray)
import Shrimp.Exception
  ( Exception (..),
    Result (Error, Ok),
    exception,
  )
import Shrimp.State
  ( State (..),
    Value (..),
    empty,
    insert,
    search,
  )
import Shrimp.SyntaxTree
  ( ArithmeticExpr (..),
    Block,
    BooleanExpr (..),
    Command (..),
    Header,
    Program,
    Variable (..),
  )
import Shrimp.Utils (liftA2, seqM2)

-- | Run a program
run :: Program -> State
run (header, block) = execute s block
  where
    s = initialize empty header

-- | Initialize a program
initialize :: State -> Header -> State
initialize s [] = s
initialize s ((IntegerDecl d) : hs) =
  case search d s of
    Just _ -> exception (MultipleVariable d)
    Nothing -> initialize s' hs
      where s' = insert d (IntegerValue 0) s
initialize s ((BooleanDecl d) : hs) =
  case search d s of
    Just _ -> exception (MultipleVariable d)
    Nothing -> initialize s' hs
      where s' = insert d (BooleanValue False) s
initialize s ((ArrayDecl d n) : hs) =
  case search d s of
    Just _ -> exception (MultipleVariable d)
    Nothing -> initialize s' hs
      where s' = insert d (ArrayValue (zeroArray n)) s

-- | Execute a program given a state
execute :: State -> Block -> State
execute s [] = s
execute s (Skip : cs) = execute s cs
execute s ((ArithmeticAssignment d a) : cs) =
  case search d s of
    Just (IntegerValue _) ->
      case evalArithmetic s a of
        Ok v -> execute s' cs
          where
            s' = insert d (IntegerValue v) s
        Error e -> exception e
    Just (BooleanValue _) -> exception (TypeMismatch d)
    Just (ArrayValue _) -> exception (TypeMismatch d)
    Nothing -> exception (UndeclaredVariable d)
execute s ((BooleanAssignment d b) : cs) =
  case search d s of
    Just (BooleanValue _) ->
      case evalBoolean s b of
        Ok t -> execute s' cs
          where
            s' = insert d (BooleanValue t) s
        Error e -> exception e
    Just (IntegerValue _) -> exception (TypeMismatch d)
    Just (ArrayValue _) -> exception (TypeMismatch d)
    Nothing -> exception (UndeclaredVariable d)
execute s ((ArrayAssignment d k a) : cs) =
  case search d s of
    Just (ArrayValue vs) ->
      case (evalArithmetic s a, evalArithmetic s k) of
        (Ok v, Ok i) -> execute s' cs
          where
            s' = insert d (ArrayValue vs') s
            vs' = case writeArray i v vs of
              Ok vs' -> vs'
              Error e -> exception e
        (Error e, _) -> exception e
        (_, Error e) -> exception e
    Just (IntegerValue _) -> exception (TypeMismatch d)
    Just (BooleanValue _) -> exception (TypeMismatch d)
    Nothing -> exception (UndeclaredVariable d)
execute s ((Branch b cs' cs'') : cs) =
  case evalBoolean s b of
    Ok True -> execute s (cs' ++ cs)
    Ok False -> execute s (cs'' ++ cs)
    Error e -> exception e
execute s (c@(Loop b cs') : cs) =
  execute s (Branch b (cs' ++ [c]) [Skip] : cs)

-- | Evaluate an arithmetic expression given a state
evalArithmetic :: State -> ArithmeticExpr -> Result Int
evalArithmetic _ (Constant v) = Ok v
evalArithmetic s (IntegerVar d) =
  case search d s of
    Just (IntegerValue v) -> Ok v
    Just (BooleanValue _) -> exception (TypeMismatch d)
    Just (ArrayValue _) -> exception (TypeMismatch d)
    Nothing -> exception (UndeclaredVariable d)
evalArithmetic s (ArrayVar d k) =
  case search d s of
    Just (ArrayValue vs) ->
      case evalArithmetic s k of
        Ok i -> readArray i vs
        Error e -> exception e
    Just (IntegerValue _) -> exception (TypeMismatch d)
    Just (BooleanValue _) -> exception (TypeMismatch d)
    Nothing -> exception (UndeclaredVariable d)
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
evalArithmetic s (Div a1 a2) = seqM2 safeDiv v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Mod a1 a2) = seqM2 safeMod v1 v2
  where
    v1 = evalArithmetic s a1
    v2 = evalArithmetic s a2
evalArithmetic s (Neg a) = negate <$> v
  where
    v = evalArithmetic s a

-- | Evaluate a boolean expression given a state
evalBoolean :: State -> BooleanExpr -> Result Bool
evalBoolean _ (Truth t) = Ok t
evalBoolean s (BooleanVar d) =
  case search d s of
    Just (IntegerValue _) -> exception $ TypeMismatch d
    Just (BooleanValue t) -> Ok t
    Just (ArrayValue _) -> exception $ TypeMismatch d
    Nothing -> exception (UndeclaredVariable d)
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

-- | Safe division
safeDiv :: Int -> Int -> Result Int
safeDiv _ 0 = Error DivisionByZero
safeDiv u v = Ok (div u v)

-- | Safe modulus
safeMod :: Int -> Int -> Result Int
safeMod _ 0 = Error DivisionByZero
safeMod u v = Ok (mod u v)
