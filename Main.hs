module Main where

import System.Environment
import Control.Monad.State

import AST
import Env
import Parser
import Eval

main = do
  args <- getArgs
  case args of
    filename:_ -> runStateT (runFile filename) defaultEnv
    _ -> do
      src <- getContents
      runStateT (runSource src) defaultEnv

