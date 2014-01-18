module AST where

type Op = String
type Identifier = String

data Expression =
  LInt Integer |
  LFloat Double |
  LString String |
  LTrue |
  LFalse |
  LNull |
  LEmpty |
  Parens E |
  Binop Op E E |
  Negative E |
  Call E [E] |
  Exec E |
  Variable Identifier |
  Dot E Identifier |
  Bracket E E |
  Record [(Identifier, E)] |
  Proc [Identifier] [Statement] |
  List [E] |
  Tuple [E] |
  Letrec E [(Identifier,E)] |
  Case E [([Pattern],E)] |
  BareCase [([Pattern],E)] |
  If [(E,E)] (Maybe E)
    deriving (Show)

data Pattern =
  PatVar Identifier |
  PatTup [Pattern] |
  PatLit Expression
    deriving (Show)
  
type E = Expression
data Statement =
  BracketLeftArrow Expression Expression Expression |
  FieldLeftArrow Expression Identifier Expression |
  ExprStatement Expression |
  BindStatement Identifier Expression |
  Import (Maybe Identifier) String deriving (Show)

data AST = AST [Statement] deriving (Show) 
