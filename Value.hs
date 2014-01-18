module Value where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Control.Concurrent.MVar
import Data.List
import AST

data Value =
  VNull |
  VTrue |
  VFalse |
  VInt Integer |
  VFloat Double |
  VString String |
  VRecord (MVar (Map String Value)) |
  VProc (Maybe String) (Map String Value) [Identifier] (Either SysProc [Statement]) |
  VDict (MVar (Map Value Value)) |
  VList [Value] |
  VTuple [Value] |
  VBlob ByteString

type SysProc = (Map String Value) -> IO Value

instance Show Value where
  show (VInt i) = show i
  show (VFloat f) = show f
  show (VNull) = "null"
  show (VString s) = show s
  show (VRecord mv) = "(record)"
  show (VProc Nothing _ _ _) = "(proc)"
  show (VProc (Just name) _ _ _) = "(proc "++name++")"
  show (VDict mv) = "(dictionary)"
  show (VTrue) = "true"
  show (VFalse) = "false"
  show (VTuple xs) = "(" ++ intercalate "," (map show xs) ++ ")"
  show (VList xs) = "[" ++ intercalate "," (map show xs) ++ "]"
  show (VBlob bs) = "(blob)"
  
