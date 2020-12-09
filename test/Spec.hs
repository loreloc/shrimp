import qualified Shrimp.Interpreter as Interpreter
import qualified Shrimp.Parser as Parser
import qualified Shrimp.State as State

testState :: String -> String -> [(String, Int)] -> IO ()
testState name filepath variables = do
  source <- readFile filepath
  let (program, message) = Parser.parse source
  if not $ null message
    then do
      putStrLn (name ++ " - parser error:")
      print message
    else do
      let state = Interpreter.run program
      if and $ checks state variables
        then putStrLn (name ++ " - passed")
        else do
          putStrLn (name ++ " - failed:")
          print state
  where
    checks s =
      map
        ( \(d, v) -> case State.search d s of
            (Just v') -> v == v'
            Nothing -> False
        )

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

main :: IO ()
main = do
  testFactorial
  testFibonacci
  testCalculator
  testEuclid
  testTripleNeg
