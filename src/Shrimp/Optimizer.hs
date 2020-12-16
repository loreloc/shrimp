module Shrimp.Optimizer where

import Shrimp.Exception
  ( Exception (DivisionByZero, InfiniteLoop),
    exception,
  )
import Shrimp.SyntaxTree
  ( ArithmeticExpr (..),
    Block,
    BooleanExpr (..),
    Command (..),
    Program,
  )

-- | Optimize a block
optimize :: Block -> Block 
optimize [] = []
optimize (Skip : cs) = optimize cs
optimize ((ArithmeticAssignment d a) : cs) = ArithmeticAssignment d a' : cs'
  where
    a' = optimizeArithmetic a
    cs' = optimize cs
optimize ((BooleanAssignment d a) : cs) = BooleanAssignment d a' : cs'
  where
    a' = optimizeBoolean a
    cs' = optimize cs
optimize ((ArrayAssignment d k a) : cs) = ArrayAssignment d k' a' : cs'
  where
    k' = optimizeArithmetic k
    a' = optimizeArithmetic a
    cs' = optimize cs
optimize ((Branch b c1 c2) : cs) =
  case b' of
    (Truth True) -> c1' ++ cs
    (Truth False) -> c2' ++ cs
    _ -> Branch b' c1' c2' : cs'
  where
    b' = optimizeBoolean b
    c1' = optimize c1
    c2' = optimize c2
    cs' = optimize cs
optimize ((Loop b c) : cs) =
  case b' of
    (Truth False) -> cs'
    (Truth True) -> exception InfiniteLoop
    _ -> Loop b' c' : cs'
  where
    b' = optimizeBoolean b
    c' = optimize c
    cs' = optimize cs

-- | Optimize an arithmetic expression
optimizeArithmetic :: ArithmeticExpr -> ArithmeticExpr
optimizeArithmetic a@(Constant _) = a
optimizeArithmetic a@(IntegerVar _) = a
optimizeArithmetic a@(ArrayVar d k) = ArrayVar d k'
  where
    k' = optimizeArithmetic k
optimizeArithmetic (Add a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Constant (v1 + v2)
    _ -> Add a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeArithmetic (Sub a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Constant (v1 - v2)
    _ -> Sub a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeArithmetic (Mul a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Constant (v1 * v2)
    _ -> Mul a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeArithmetic (Div a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) ->
      if v2 == 0
        then exception DivisionByZero
        else Constant (v1 `div` v2)
    _ -> Div a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeArithmetic (Mod a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) ->
      if v2 == 0
        then exception DivisionByZero
        else Constant (v1 `mod` v2)
    _ -> Mod a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeArithmetic (Neg a) =
  case a' of
    (Constant v) -> Constant (- v)
    _ -> Neg a'
  where
    a' = optimizeArithmetic a

-- | Optimize a boolean expression
optimizeBoolean :: BooleanExpr -> BooleanExpr
optimizeBoolean b@(Truth _) = b
optimizeBoolean b@(BooleanVar _) = b
optimizeBoolean (Not b) =
  case b' of
    (Truth t) -> Truth (not t)
    _ -> Not b'
  where
    b' = optimizeBoolean b
optimizeBoolean (Or b1 b2) =
  case (b1', b2') of
    (Truth t1, Truth t2) -> Truth (t1 || t2)
    _ -> Or b1' b2'
  where
    b1' = optimizeBoolean b1
    b2' = optimizeBoolean b2
optimizeBoolean (And b1 b2) =
  case (b1', b2') of
    (Truth t1, Truth t2) -> Truth (t1 && t2)
    _ -> And b1' b2'
  where
    b1' = optimizeBoolean b1
    b2' = optimizeBoolean b2
optimizeBoolean (Equal a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 == v2)
    _ -> Equal a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeBoolean (NotEqual a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 /= v2)
    _ -> NotEqual a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeBoolean (Less a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 < v2)
    _ -> Less a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeBoolean (Greater a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 > v2)
    _ -> Greater a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeBoolean (LessEqual a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 <= v2)
    _ -> LessEqual a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
optimizeBoolean (GreaterEqual a1 a2) =
  case (a1', a2') of
    (Constant v1, Constant v2) -> Truth (v1 >= v2)
    _ -> GreaterEqual a1' a2'
  where
    a1' = optimizeArithmetic a1
    a2' = optimizeArithmetic a2
