import Data.Maybe (fromJust, isJust)
import qualified Shrimp.Interpreter as Interpreter
import qualified Shrimp.Parser as Parser
import qualified Shrimp.State as State

test :: String -> String -> [(String, Int)] -> IO ()
test name filepath variables = do
  source <- readFile filepath
  let (program, message) = Parser.parse source
  if not $ null message
    then do
      putStrLn (name ++ " - parser error:")
      print message
    else do
      let state = Interpreter.run program
      if not $ and [isJust (State.search d state) | (d, v) <- variables]
        then putStrLn (name ++ " - undefined result")
        else do
          if and [fromJust (State.search d state) == v | (d, v) <- variables]
            then putStrLn (name ++ " - passed")
            else putStrLn (name ++ " - result mismatch")

testFactorial :: IO ()
testFactorial =
  test "Factorial" "examples/factorial.shr" [("x", 120), ("n", 5), ("i", 6)]

testFibonacci :: IO ()
testFibonacci =
  test "Fibonacci" "examples/fibonacci.shr" [("x", 55), ("f", 34), ("g", 55), ("k", 1)]

testCalculator :: IO ()
testCalculator =
  test "Calculator" "examples/calculator.shr" [("x", 10), ("y", 2), ("op", 1), ("result", 8)]

testEuclid :: IO ()
testEuclid =
  test "Euclid" "examples/euclid.shr" [("a", 21), ("b", 0), ("t", 21)]

testTripleNeg :: IO ()
testTripleNeg =
  test "TripleNeg" "examples/tripleneg.shr" [("x", -1816), ("y", 42)]

main :: IO ()
main = do
  testFactorial
  testFibonacci
  testCalculator
  testEuclid
  testTripleNeg
