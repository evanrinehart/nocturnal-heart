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

instance Eq Value where
  VNull == VNull = True
  VTrue == VTrue = True
  VTrue == VFalse = False
  VFalse == VTrue = False
  VFalse == VFalse = False
  VInt i == VInt j = i == j
  VFloat i == VFloat j = i == j
  VString s1 == VString s2 = s1 == s2
  VTuple vs1 == VTuple vs2 = vs1 == vs2
  VBlob b1 == VBlob b2 = b1 == b2
  _ == _ = error "cannot compare these two values"

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
  
instance Ord Value where
  VInt i `compare` VInt j = i `compare` j
  VFloat i `compare` VFloat j = i `compare` j
  VNull `compare` VNull = EQ
  compare VTrue VTrue = EQ
  compare VTrue VFalse = GT
  compare VFalse VTrue = LT
  compare VFalse VFalse = EQ
  compare (VString s1) (VString s2) = compare s1 s2
  compare (VTuple vs1) (VTuple vs2) = compare vs1 vs2
  compare _ _ = error "cannot compare these two values"
