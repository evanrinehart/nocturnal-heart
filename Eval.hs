module Eval where

import AST
import Value
import Env
import Parser

import Control.Monad
import Control.Monad.State
import Data.Map
import qualified Data.Map as M

runFile :: String -> Env Value
runFile filename = do
  src <- io (readFile filename)
  v <- runSource src
  return v

runSource :: String -> Env Value
runSource src = case parse src of
  Left err -> envError ("Syntax Error: "++err)
  Right (AST statements) -> runAST statements

runAST :: [Statement] -> Env Value
runAST [] = return VNull
runAST [final] = runStat final
runAST (stat:stats) = do
  runStat stat
  runAST stats

runStat :: Statement -> Env Value
runStat stat = case stat of
  ExprStatement e -> eval e
  BindStatement var e -> do
    v <- eval e
    -- fixup proc heads in v with v
    envWrite var v
    return VNull
  Import mName filename -> do
    v <- runFile filename
    -- if v is a record, union that into the env
    return VNull
  BracketLeftArrow e1 e2 e3 -> do
    obj <- eval e1
    key <- eval e2
    v <- eval e3
    -- if obj is tuple, dictionary, ok
    return VNull
  FieldLeftArrow e1 field e2 -> do
    obj <- eval e1
    v <- eval e2
    -- if obj is record, ok
    return VNull

eval :: Expression -> Env Value
eval (LInt i) = return (VInt i)
eval (LFloat f) = return (VFloat f)
eval (LString s) = return (VString s)
eval LTrue = return VTrue
eval LFalse = return VFalse
eval LNull = return VNull
eval LEmpty = return (VList [])
eval (Parens e) = eval e
eval (Negative e) = do
  v <- eval e
  neg v
eval (Binop op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ case op of
    "+" -> math "+" (+) (+) v1 v2
    "-" -> math "-" subtract subtract v1 v2
    "*" -> math "*" (*) (*) v1 v2
    "/" -> division v1 v2
    "%" -> modulo v1 v2
    "^" -> power v1 v2
    "<" -> comp "<" (<) (<) v1 v2
    "<=" -> comp "<=" (<=) (<=) v1 v2
    ">" -> comp ">" (>) (>) v1 v2
    ">=" -> comp ">=" (>=) (>=) v1 v2
    "!=" -> comp "!=" (/=) (/=) v1 v2
    "==" -> comp "==" (==) (==) v1 v2
    "&&" -> land v1 v2
    "||" -> lor v1 v2

eval (Call e es) = do
  p <- eval e
  args <- mapM eval es
  case p of
    VProc mName closures params body -> do
      when (length args /= length params) (error $ "wrong number of arguments")
      env <- envSave
      forM_ (zip params args) $ \(name,value) -> do
        envWrite name value
      v <- case body of
        Left sys -> do
          env <- envSave
          io (sys env)
        Right stats -> do
          runAST stats
      envLoad env
      return v
    _ -> error "calling a non-proc value"
eval (Variable x) = do
  mv <- envRead x
  case mv of
    Nothing -> error $ "variable "++x++" not defined"
    Just v -> return v
eval (Tuple es) = do
  vs <- mapM eval es
  return (VTuple vs)
eval (List es) = do
  vs <- mapM eval es
  return (VList vs)
eval (Proc params stats) = do
  let freeVarNames = findFreeVars params stats
  freeVars <- forM freeVarNames $ \name -> do
    mval <- envRead name
    case mval of
      Nothing -> error "variable not defined"
      Just v -> return v
  let closures = M.fromList (zip freeVarNames freeVars)
  return (VProc Nothing closures params (Right stats))

{-
  Dot E Identifier |
  Bracket E E |
  Record [(Identifier, E)] |
  Proc [Identifier] [Statement] |
  Letrec E [(Identifier,E)] |
  Case E [([Pattern],E)] |
  BareCase [([Pattern],E)] |
  If [(E,E)] (Maybe E)
-}
eval _ = error "unimplemented expression"

findFreeVars :: [String] -> [Statement] -> [String]
findFreeVars ambient stats = []

neg v = case v of
  VInt i -> return (VInt (-i))
  VFloat d -> return (VFloat (-d))
  _ -> error ("used negative on "++show v)

math o f g (VInt n1) (VInt n2) = VInt (f n1 n2)
math o f g (VFloat n1) (VFloat n2) = VFloat (g n1 n2)
math o _ _ _ _ = error $ o++" used on invalid arguments"

comp o f g (VInt n1) (VInt n2) = if f n1 n2 then VTrue else VFalse
comp o f g (VFloat n1) (VFloat n2) = if g n1 n2 then VTrue else VFalse
comp o _ _ _ _ = error $ o++" used on invalid arguments"

division (VInt n1) (VInt n2) = VInt (div n1 n2)
division (VFloat n1) (VFloat n2) = VFloat (n1 / n2)
division _ _ = error "/ used on invalid arguments"

power (VInt n1) (VInt n2) = VInt (n1 ^ n2)
power (VFloat n1) (VFloat n2) = VFloat (n1 ** n2)
power _ _ = error "^ used on invalid arguments"

modulo (VInt n1) (VInt n2) = VInt (mod n1 n2)
modulo (VFloat n1) (VFloat n2) = error "% used on floats"
modulo _ _ = error "% used on invalid arguments"

land VTrue VTrue = VTrue
land VFalse VTrue= VFalse
land VTrue VFalse = VFalse
land VFalse VFalse = VFalse
land _ _ = error "&& used on invalid arguments"

lor VTrue VTrue = VTrue
lor VFalse VTrue= VTrue
lor VTrue VFalse = VTrue
lor VFalse VFalse = VFalse
lor _ _ = error "|| used on invalid arguments"
