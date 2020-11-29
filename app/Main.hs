module Main where

import Grammar
    ( Program(Program),
      Command(Assignment),
      VariableDecl(IntegerDecl),
      ArithmeticExpr(Constant) )

main :: IO ()
main = do
    let p = Program [IntegerDecl "ciao" (Constant 1816)] [Assignment "ciao" (Constant 42)]
    print p
