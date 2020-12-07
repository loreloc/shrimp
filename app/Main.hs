module Main where

import qualified Shrimp.Interpreter as Interpreter
import qualified Shrimp.Parser as Parser
import System.Environment (getArgs)

help :: IO ()
help = do
  putStrLn "🦐 The Shrimp Interpreter 🦐"
  putStrLn "Usage:\tshrimp <program>.shr"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then help
    else do
      source <- readFile $ head args
      let program = Parser.parse source
      let state = Interpreter.run program
      print state
