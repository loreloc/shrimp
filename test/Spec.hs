import Data.Maybe (fromJust, isJust)
import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul),
    BooleanExpr (LessEqual),
    Command (Assignment, Loop),
    Program (Program),
    VariableDecl (IntegerDecl),
  )
import Shrimp.Interpreter
  ( execute,
  )
import Shrimp.State
  ( empty,
    search,
  )

programFactorial :: Program
programFactorial =
  Program
    [ IntegerDecl "i" (Constant 1),
      IntegerDecl "n" (Constant 5),
      IntegerDecl "x" (Constant 1)
    ]
    [ Loop
        (LessEqual (Identifier "i") (Identifier "n"))
        [ Assignment "x" (Mul (Identifier "x") (Identifier "i")),
          Assignment "i" (Add (Identifier "i") (Constant 1))
        ]
    ]

testFactorial :: IO ()
testFactorial = do
  let s = execute empty programFactorial
  let v = search "x" s
  if isJust v
    then
      if fromJust v == 120
        then print "Factorial - passed"
        else error "Factorial - result mismatch"
    else error "Factorial - undefined result"

main :: IO ()
main = do
  testFactorial
