import Data.Maybe (fromJust, isJust)
import qualified Shrimp.Interpreter as Interpreter
import qualified Shrimp.Parser as Parser
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

testEuclid :: IO ()
testEuclid = do
  source <- readFile "examples/euclid.shr"
  let (program, _) = Parser.parse source
  let state = Interpreter.run program
  let a = State.search "a" state
  let b = State.search "b" state
  if isJust a && isJust b
    then
      if fromJust a == 21 && fromJust b == 0
        then print "Euclid - passed"
        else error "Euclid - result mismatch"
    else error "Euclid - undefined result"

testTripleNeg :: IO ()
testTripleNeg = do
  source <- readFile "examples/tripleneg.shr"
  let (program, _) = Parser.parse source
  let state = Interpreter.run program
  let x = State.search "x" state
  if isJust x
    then
      if fromJust x == (-1816)
        then print "TripleNeg - passed"
        else error "TripleNeg - result mismatch"
    else error "TripleNeg - undefined result"

main :: IO ()
main = do
  testFactorial
  testFibonacci
  testCalculator
  testEuclid
  testTripleNeg
