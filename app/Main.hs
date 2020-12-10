module Main where

import Shrimp.Exception (Result (Error, Ok), exception)
import Shrimp.Interpreter (run)
import Shrimp.Parser (parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then help
    else do
      source <- readFile $ head args
      interpret source

help :: IO ()
help = do
  putStrLn "🦐 The Shrimp Interpreter 🦐"
  putStrLn "Usage:\tshrimp <program>.shr"

interpret :: String -> IO ()
interpret source =
  case parse source of
    Ok (program, message) -> do
      if null message
        then do
          let state = run program
          putStrLn "Memory state:"
          print state
        else do
          putStrLn "Parsing failed:"
          print message
    Error e -> exception e
