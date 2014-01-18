module Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

import Value

type Context = Map String Value
type Env = StateT Context IO

envError :: String -> Env a
envError msg = error msg

io :: IO a -> Env a
io = liftIO

blankEnv :: Map String Value
blankEnv = M.empty

defaultEnv :: Map String Value
defaultEnv = M.fromList [("print", sysPrint)]

sysPrint = VProc (Just "print") blankEnv ["x"] (Left act) where
  act args = case M.lookup "x" args of
    Just x -> do
      print x
      return VNull
    Nothing -> error "argument not provided (internal bug)"
  

envWrite :: String -> Value -> Env ()
envWrite name v = modify (M.insert name v)

envRead :: String -> Env (Maybe Value)
envRead name = gets (M.lookup name)

envSave :: Env Context
envSave = get

envLoad :: Context -> Env ()
envLoad = put
