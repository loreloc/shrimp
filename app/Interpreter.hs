module Interpreter where

import Control.Exception
import Exception
import Grammar
    ( ArithmeticExpr(Add, Sub, Mul, Constant, Identifier),
      BooleanExpr(Boolean, Not, Or, And, Equal, LessEqual),
      VariableDecl(IntegerDecl),
      Command(Skip, Assignment, Branch, Loop),
      Program(Program) )

-- |The state (memory) of the interpreter
type State = [(String, Int)]

-- |Augment a state given a variable declaration
augmentState :: State -> VariableDecl -> State
augmentState s (IntegerDecl d a) = case r of 
        Just _ -> throw (MultipleDeclaration d)
        Nothing -> (d, v) : s
    where r = lookup d s
          v = evaluateArithmExp a s

-- |Evaluate arithmetic expressions given a state
evaluateArithmExp :: ArithmeticExpr -> State -> Int
evaluateArithmExp (Add a1 a2) s = v1 + v2
    where v1 = evaluateArithmExp a1 s
          v2 = evaluateArithmExp a2 s
evaluateArithmExp (Sub a1 a2) s = v1 - v2
    where v1 = evaluateArithmExp a1 s
          v2 = evaluateArithmExp a2 s
evaluateArithmExp (Mul a1 a2) s = v1 * v2
    where v1 = evaluateArithmExp a1 s
          v2 = evaluateArithmExp a2 s
evaluateArithmExp (Constant i) _ = i
evaluateArithmExp (Identifier d) s = case r of
        Just v -> v
        Nothing -> throw (UndeclaredVariable d)
    where r = lookup d s

-- |Execute a program given a state
execute :: State -> Program -> State
execute s (Program vs cs) = executeCommands s' cs
    where s' = executeVariables s vs

-- |Execute the variables declaration
executeVariables :: State -> [VariableDecl] -> State
executeVariables s (v : vs) = s''
    where s' = augmentState s v
          s'' = executeVariables s' vs

-- |Execute the commands section
executeCommands :: State -> [Command] -> State
executeCommands s [] = s
executeCommands s (Skip : cs) = executeCommands s cs
