module Shrimp.Interpreter where

import Shrimp.Exception
  ( Result (Error, Ok),
    RuntimeException (MultipleDeclaration, UndeclaredVariable),
    exception,
  )
import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul, Sub),
    BooleanExpr (And, Boolean, Equal, LessEqual, Not, Or),
    Command (Assignment, Branch, Loop, Skip),
    Program (Program),
    VariableDecl (IntegerDecl),
  )
import Shrimp.State
  ( State,
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
execute :: State -> Program -> State
execute s (Program vs cs) = executeCommands s' cs
  where
    s' = executeVariables s vs

-- | Execute the variables declaration
executeVariables :: State -> [VariableDecl] -> State
executeVariables s [] = s
executeVariables s (v : vs) = s''
  where
    s' = augmentState s v
    s'' = executeVariables s' vs

-- | Execute the commands section
executeCommands :: State -> [Command] -> State
executeCommands s [] = s
executeCommands s (Skip : cs) = executeCommands s cs
executeCommands s ((Assignment d a) : cs) =
  case search d s of
    Just _ -> case evalArithmetic s a of
      Ok v -> executeCommands s' cs
        where
          s' = insert d v s
      Error e -> exception e
    Nothing -> exception (UndeclaredVariable d)
executeCommands s ((Branch b cs' cs'') : cs) =
  case evalBoolean s b of
    Ok True -> executeCommands s (cs' ++ cs)
    Ok False -> executeCommands s (cs'' ++ cs)
    Error e -> exception e
executeCommands s ((Loop b cs') : cs) =
  case evalBoolean s b of
    Ok True -> executeCommands s' (Loop b cs' : cs)
      where
        s' = executeCommands s cs'
    Ok False -> executeCommands s cs
    Error e -> exception e

-- | Augment a state by declaring a variable
augmentState :: State -> VariableDecl -> State
augmentState s (IntegerDecl d a) =
  case search d s of
    Just _ -> exception (MultipleDeclaration d)
    Nothing -> case evalArithmetic s a of
      Ok v -> insert d v s
      Error e -> exception e
