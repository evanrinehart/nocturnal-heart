module Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.IORef

import Value

type Env = StateT Context IO

envError :: String -> Env a
envError msg = error msg

io :: IO a -> Env a
io = liftIO

blankContext :: Context
blankContext = M.empty

defaultEnv :: Context
defaultEnv = M.fromList [("print", Right sysPrint)]

sysPrint = VProc (Just "print") blankContext ["x"] (Left act) where
  act context [x] = do
    print x
    return VNull
  act _ _ = error "wrong number of arguments"

envWrite :: String -> Value -> Env ()
envWrite name v = modify (M.insert name (Right v))

envWriteLate :: String -> Value -> Env ()
envWriteLate name v = do
  mlr <- gets (M.lookup name)
  case mlr of
    Nothing -> error "late write to missing variable"
    Just lr -> case lr of
      Left ref -> io (writeIORef ref (Normal v))
      Right _ -> error "late write to normal variable"

envSetupIndirection :: String -> Indirection -> Env ()
envSetupIndirection name ind = do
  mlr <- gets (M.lookup name)
  case mlr of
    Just _ -> error "refuse to setup indirection at an initialized location"
    Nothing -> do
      ref <- io (newIORef ind)
      modify (M.insert name (Left ref))

envReadRaw :: String -> Env (Maybe (Either (IORef Indirection) Value))
envReadRaw name = gets (M.lookup name)

envRead :: String -> Env (Maybe Value)
envRead name = do
  mlr <- gets (M.lookup name)
  case mlr of
    Nothing -> return Nothing
    Just lr -> case lr of
      Left ref -> do
        ind <- io (readIORef ref)
        case ind of
          JustCrash -> error "no one should be here"
          LetrecRestriction -> error "letrec restriction violated"
          RecordRestriction -> error "using this too early"
          Normal v -> return (Just v)
      Right v -> return (Just v)

envSave :: Env Context
envSave = get

envLoad :: Context -> Env ()
envLoad = put
