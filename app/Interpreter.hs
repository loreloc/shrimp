module Interpreter where

import Control.Exception ( throw )
import Control.Applicative (Applicative(liftA2))
import Exception
    ( RuntimeException(MultipleDeclaration, UndeclaredVariable),
      Result(Ok, Error),  )
import Grammar
    ( ArithmeticExpr(Add, Sub, Mul, Constant, Identifier),
      VariableDecl(IntegerDecl),
      Command(Skip, Assignment, Branch, Loop),
      Program(Program) )

-- |The state (memory) of the interpreter
type State = [(String, Int)]

-- |Evaluate an arithmetic expression given a state
evalArithmetic :: State -> ArithmeticExpr -> Result Int
evalArithmetic _ (Constant v) = Ok v
evalArithmetic s (Identifier d) =
    case lookup d s of
        Just v -> Ok v
        Nothing -> Error (UndeclaredVariable d)
evalArithmetic s (Add a1 a2) = liftA2 (+) v1 v2
    where v1 = evalArithmetic s a1
          v2 = evalArithmetic s a2
evalArithmetic s (Sub a1 a2) = liftA2 (-) v1 v2
    where v1 = evalArithmetic s a1
          v2 = evalArithmetic s a2
evalArithmetic s (Mul a1 a2) = liftA2 (*) v1 v2
    where v1 = evalArithmetic s a1
          v2 = evalArithmetic s a2

-- |Execute a program given a state
execute :: State -> Program -> State
execute s (Program vs cs) = executeCommands s' cs
    where s' = executeVariables s vs

-- |Execute the variables declaration
executeVariables :: State -> [VariableDecl] -> State
executeVariables s [] = s
executeVariables s (v : vs) = s''
    where s' = augmentState s v
          s'' = executeVariables s' vs

-- |Execute the commands section
executeCommands :: State -> [Command] -> State
executeCommands s [] = s
executeCommands s (Skip : cs) = executeCommands s cs
executeCommands s ((Assignment d a) : cs) = []  -- TODO
executeCommands s ((Branch b cs' cs'') : cs) = []  -- TODO
executeCommands s ((Loop b cs') : cs) = [] -- TODO

-- |Augment a state by declaring a variable
augmentState :: State -> VariableDecl -> State
augmentState s (IntegerDecl d a) =
    case p of
        Just _ -> throw (MultipleDeclaration d)
        Nothing -> case q of
            Ok v -> (d, v) : s
            Error e -> throw e
    where p = lookup d s
          q = evalArithmetic s a
