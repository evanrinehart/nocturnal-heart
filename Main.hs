module Main where

import Parser
import AST

main = do
  src <- readFile "source"
  print (parse src)
