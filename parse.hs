module Main where

import System.Environment
import AST
import Parser

main = do
  args <- getArgs
  src <- case args of
    filename:_ -> readFile filename
    _ -> getContents
  case parse src of
    Left err -> putStrLn ("ERROR: "++err)
    Right ast -> print ast
