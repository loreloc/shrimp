module Main where

import Shrimp

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
    Ok (code, message) -> do
      if null message
        then do
          let state = run code
          putStrLn "Memory state:"
          print state
        else do
          putStrLn "Parsing failed:"
          print message
    Error e -> exception e
