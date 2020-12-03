module Main where

import Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul),
    BooleanExpr (LessEqual),
    Command (Assignment, Loop),
    Program (Program),
    VariableDecl (IntegerDecl),
  )
import Interpreter
  ( emptyState,
    execute,
  )

main :: IO ()
main = do
  let p =
        Program
          [IntegerDecl "i" (Constant 1), IntegerDecl "n" (Constant 5), IntegerDecl "x" (Constant 1)]
          [ Loop
              (LessEqual (Identifier "i") (Identifier "n"))
              [ Assignment "x" (Mul (Identifier "x") (Identifier "i")),
                Assignment "i" (Add (Identifier "i") (Constant 1))
              ]
          ]
  let s = execute emptyState p
  print p
  print s
