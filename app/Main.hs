module Main where

import Shrimp.Exception (Result (Error, Ok), exception)
import Shrimp.Interpreter (run)
import Shrimp.Parser (parse)

main :: IO ()
main = do
  filepath <- help
  source <- readFile filepath
  interpret source

help :: IO String
help = do
  putStrLn "The Shrimp Interpreter\nInsert the path of the source file: "
  getLine

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
