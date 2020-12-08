import Data.Maybe (fromJust, isJust)
import qualified Shrimp.Parser as Parser
import qualified Shrimp.Interpreter as Interpreter
import qualified Shrimp.State as State

testFactorial :: IO ()
testFactorial = do
  source <- readFile "examples/factorial.shr"
  let (program, _) = Parser.parse source
  let state = Interpreter.run program
  let x = State.search "x" state
  if isJust x
    then
      if fromJust x == 120
        then print "Factorial - passed"
        else error "Factorial - result mismatch"
    else error "Factorial - undefined result"

testFibonacci :: IO ()
testFibonacci = do
  source <- readFile "examples/fibonacci.shr"
  let (program, _) = Parser.parse source
  let state = Interpreter.run program
  let x = State.search "x" state
  let f = State.search "f" state
  let g = State.search "g" state
  if isJust x && isJust f && isJust g
  then
    if fromJust x == 55 && fromJust f == 34 && fromJust g == 55
      then print "Fibonacci - passed"
      else error "Fibonacci - result mismatch"
  else error "Fibonacci - undefined result"

testCalculator :: IO ()
testCalculator = do
  source <- readFile "examples/calculator.shr"
  let (program, _) = Parser.parse source
  let state = Interpreter.run program
  let x = State.search "result" state
  if isJust x
    then
      if fromJust x == 8
        then print "Calculator - passed"
        else error "Calculator - result mismatch"
    else error "Calculator - undefined result"

main :: IO ()
main = do
  testFactorial
  testFibonacci
  testCalculator
