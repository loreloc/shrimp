module Interpreter where

import Text.Printf
import Data.List
import Grammar
    ( ArithmeticExpr(Add, Sub, Mul, Constant, Identifier),
      Program(Program),
      VariableDecl(IntegerDecl),
      Command(Skip) )

-- |The state (memory) of the interpreter
type State = [(String, Int)]

-- |Evaluate arithmetic expressions given a state
evaluateArithmExp :: ArithmeticExpr -> State -> Int
evaluateArithmExp (Add aexp1 aexp2) state = i1 + i2
    where i1 = evaluateArithmExp aexp1 state
          i2 = evaluateArithmExp aexp2 state
evaluateArithmExp (Sub aexp1 aexp2) state = i1 - i2
    where i1 = evaluateArithmExp aexp1 state
          i2 = evaluateArithmExp aexp2 state
evaluateArithmExp (Mul aexp1 aexp2) state = i1 * i2
    where i1 = evaluateArithmExp aexp1 state
          i2 = evaluateArithmExp aexp2 state
evaluateArithmExp (Constant i) _ = i
evaluateArithmExp (Identifier id) state = case r of
        Nothing -> error $ printf "Undefined variable '%s'" id
        Just (_, v) -> v
    where r = find (\(d, _) -> id == d) state

-- |Update the state
updateState :: State -> VariableDecl -> State
updateState [] (IntegerDecl id aexp) = [(id, value)]
    where value = evaluateArithmExp aexp []

-- |Load the variables
loadVariables :: State -> [VariableDecl] -> State
loadVariables state (v : vs) = state''
    where state' = updateState state v
          state'' = loadVariables state' vs

-- Execute a program given a state
execute :: State -> Program -> State
execute state (Program [] []) = state
execute state (Program [] (Skip : cs)) = execute state (Program [] cs)
execute state (Program vs cs) = execute (loadVariables state vs) (Program [] cs)
