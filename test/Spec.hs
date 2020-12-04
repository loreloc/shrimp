import qualified Data.Map as Map
  ( lookup,
  )
import Data.Maybe ( fromJust, isJust )
import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul),
    BooleanExpr (LessEqual),
    Command (Assignment, Loop),
    Program (Program),
    VariableDecl (IntegerDecl),
  )
import Shrimp.Interpreter
  ( emptyState,
    execute,
  )
import Test.HUnit
    ( assertBool,
      assertEqual,
      runTestTT,
      Counts,
      Test(TestList, TestCase) )

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

testFactorial :: Test
testFactorial =
  TestCase
    ( do
        let s = execute emptyState programFactorial
        let v = Map.lookup "x" s
        assertBool "Factorial - result is undefined" (isJust v)
        assertEqual "Factorial - result mismatch" (fromJust v) 120
    )

main :: IO Counts
main = do
  runTestTT (TestList [testFactorial])
