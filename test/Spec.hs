import Shrimp.Grammar
  ( ArithmeticExpr (..),
    Block,
    BooleanExpr (..),
    Command (..),
  )
import Shrimp.Exception (Result (Error, Ok), exception)
import Shrimp.Interpreter (run)
import Shrimp.Optimizer (optimize)
import Shrimp.Parser (parse)
import Shrimp.State (search)

testState :: String -> String -> [(String, Int)] -> IO ()
testState name filepath variables = do
  source <- readFile filepath
  case parse source of
    Ok (program, message) -> do
      if not $ null message
        then do
          putStrLn (name ++ " - parsing failed:")
          print message
        else do
          let state = run program
          if and $ checks state variables
            then putStrLn (name ++ " - passed")
            else do
              putStrLn (name ++ " - failed:")
              print state
    Error e -> exception e
  where
    checks s =
      map
        ( \(d, v) -> case search d s of
            (Just v') -> v == v'
            Nothing -> False
        )

testOptimization :: String -> Block -> Block -> IO ()
testOptimization name source target = do
  let result = optimize source
  if result == target
    then do
      putStrLn (name ++ " - passed")
    else do
      putStrLn (name ++ " - failed:")
      print result

testFactorial :: IO ()
testFactorial = testState "Factorial" filepath variables
  where
    filepath = "examples/factorial.shr"
    variables = [("x", 120), ("n", 5), ("i", 6)]

testFibonacci :: IO ()
testFibonacci = testState "Fibonacci" filepath variables
  where
    filepath = "examples/fibonacci.shr"
    variables = [("x", 55), ("f", 34), ("g", 55), ("k", 1)]

testCalculator :: IO ()
testCalculator = testState "Calculator" filepath variables
  where
    filepath = "examples/calculator.shr"
    variables = [("x", 10), ("y", 2), ("op", 1), ("result", 8)]

testEuclid :: IO ()
testEuclid = testState "Euclid" filepath variables
  where
    filepath = "examples/euclid.shr"
    variables = [("a", 21), ("b", 0), ("t", 21)]

testTripleNeg :: IO ()
testTripleNeg = testState "TripleNeg" filepath variables
  where
    filepath = "examples/tripleneg.shr"
    variables = [("x", -1816), ("y", 42)]

testArithmetic :: IO ()
testArithmetic = testOptimization "ArithmeticOpt" source target
  where
    source =
      [ Assignment "x" (Add (Mul (Constant 5) (Constant 2)) (Constant 6)),
        Assignment "y" (Div (Identifier "x") (Sub (Constant 9) (Constant 1)))
      ]
    target =
      [ Assignment "x" (Constant 16),
        Assignment "y" (Div (Identifier "x") (Constant 8))
      ]

testBoolean :: IO ()
testBoolean = testOptimization "BooleanOpt" source target
  where
    source =
      [ Branch (Not (Not (Or (Boolean True) (Boolean False))))
        [Skip, Assignment "x" (Constant 1), Skip]
        [Assignment "y" (Constant 2)]
      ]
    target =
      [ Assignment "x" (Constant 1)
      ]
    

main :: IO ()
main = do
  testFactorial
  testFibonacci
  testCalculator
  testEuclid
  testTripleNeg
  testArithmetic
  testBoolean
