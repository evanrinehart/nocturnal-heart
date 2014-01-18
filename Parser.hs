module Parser where

import AST

import Text.Parsec
import Text.Parsec.Expr
import Data.Maybe

import Debug.Trace


type Parser = Parsec String ()

parse :: String -> Either String AST
parse x = case runParser parser () "source" x of
  Left err -> Left (show err)
  Right ast -> Right ast

parser :: Parser AST
parser = do
  statements <- procbody
  eof
  return (AST statements) 

statSep :: Parser ()
statSep = many1 p >> return () where
  p = do
    newline <|> char ';'
    skipMany whitespace

whitespaceChars = oneOf " "
whitespace = many1 whitespaceChars

statement :: Parser Statement
statement = choice
  [try importStatement,
   try bindStatement,
   try fieldLeftArrow,
   try bracketLeftArrow,
   exprStatement]
   

importStatement = do
  string "import"
  whitespace
  path <- stringLit
  name <- optionMaybe . try $ do
    whitespace
    string "as"
    whitespace
    name <- identifier
    whitespace
    return name
  return (Import name path)

bindStatement = do
  v <- identifier <?> "identifier"
  optional whitespace
  char '='
  optional whitespace
  e <- expression <?> "expression"
  return (BindStatement v e)

exprStatement = do
  e <- expression
  return (ExprStatement e)

operatorChar :: Parser Char
operatorChar = oneOf "+-*/|&^%<>="

hardTrailing = do
  ws <- whitespace
  notFollowedBy (alphaNum <|> oneOf "_$([{")
  return ws

trailing :: Parser String
trailing = do
  ms <- optionMaybe (try hardTrailing)
  return (fromMaybe "" ms)
  

term = choice
  [
    parens,
    variable,
    list,
    tuple,
    proc,
    record,
    letrec,
    ifexpr,
    caseexpr,
    basecase,
    literal
  ]

expression = buildExpressionParser table term where
  table =
   [[opDotBrakChain],
    [opNeg],
    [opPower],
    [opApply],
    [opExec],
    [opMul, opDiv],
    [opAdd, opSub],
    [opEQ, opNEQ, opGT, opGTE, opLT, opLTE],
    [opAnd, opOr]]

brackets = do
  char '['
  spaces
  e <- expression
  char ']'
  return (Left e)

dots = do
  char '.'
  i <- identifier
  return (Right i)

opDotBrakChain = Postfix $ do
  dotbraks <- many1 (brackets <|> dots)
  trailing
  let
    comb (Left e2) e1 = Bracket e1 e2
    comb (Right f) e1 = Dot e1 f
    chain base = foldr comb base (reverse dotbraks)
  return chain

{-
opDotBrakChain = Postfix $ do
  dotbraks <- many1 (brackets <|> dots)
  trailing
  return (\base -> -- attach all the dotbraks to base
  -}

opNeg = Prefix $ do
  negation
  return Negative

opApply = Infix p AssocLeft where
  p = try $ do
    many1 whitespace
    notFollowedBy (oneOf "+-*/|&^,]}<=!")
    return collapse
  collapse (Call e as) b = Call e (as++[b])
  collapse e b = Call e [b]

opPower = Infix p AssocLeft where
  p = do
    char '^'
    return (Binop "^")

opExec = Prefix p where
  p = try $ do
    string "exec"
    many1 whitespace
    return Exec

opBinary opParser op = Infix p AssocLeft where
  p = try $ do
    skipMany whitespace
    opParser
    skipMany whitespace
    return (Binop op)

opMul = opBinary (char '*') "*"
opDiv = opBinary (char '/') "/"
opAdd = opBinary (char '+') "+"
opSub = opBinary subtraction "-"
opAnd = opBinary (string "&&") "&&"
opOr = opBinary (string "||") "||"

opEQ = opBinary (string "==") "=="
opNEQ = opBinary (string "!=") "!=" 
opLT = opBinary lessThan "<" 
opLTE = opBinary (try $ string "<=") "<=" 
opGT = opBinary (char '>') ">" 
opGTE = opBinary (string ">=") ">=" 

lessThan = try $ do
  char '<'
  notFollowedBy (char '-')
  return '<'

negation = do
  char '-'
  notFollowedBy (char '>' <|> whitespaceChars)
  return '-'

subtraction = try $ do
  char '-'
  notFollowedBy (char '>')
  return '-'

parens = try $ do
  char '('
  spaces
  e <- expression
  spaces
  char ')'
  trailing
  return (Parens e)

variable = do
  v <- identifier
  trailing
  return (Variable v)

literal = do
  l <- choice [
    try double,
    try integer,
    string "true" >> return LTrue,
    string "false" >> return LFalse,
    string "null" >> return LNull,
    stringLitExpr
    ]
  trailing
  return l

integer = do
  minus <- fmap (fromMaybe "") (optionMaybe (string "-"))
  digits <- many1 digit
  return (LInt (read (minus++digits)))

double = do
  let maybeString p = fmap (fromMaybe "") (optionMaybe p)
  minus <- maybeString (string "-")
  ldigits <- many1 digit
  char '.'
  rdigits <- many1 digit 
  exponent <- maybeString $ do
    oneOf "eE"
    minus <- maybeString (string "-")
    power <- many1 digit
    return ("e"++minus++power)
  return (LFloat (read (minus++ldigits++"."++rdigits++exponent)))

stringLit = do
  char '"'
  manyTill anyChar (try (char '"'))

stringLitExpr = fmap LString stringLit

identifier = do
  notFollowedBy keyword
  lead <- letter <|> char '_' <|> char '$'
  rest <- many (letter <|> digit <|> char '_' <|> char '$')
  return (lead:rest)

keywords = ["true", "false", "null", "record", "case", "if", "else", "exec", "letrec"]

keyword = do
  choice (map string keywords)
  notFollowedBy (alphaNum <|> char '_')

listSep = do
  char ','
  spaces

list = do
  char '['
  spaces
  expressions <- sepBy expression listSep
  spaces
  char ']'
  trailing
  case expressions of
    [] -> return LEmpty
    es -> return (List expressions)

tuple = try $ do
  char '('
  spaces
  expressions <- sepBy1 expression listSep
  spaces
  char ')'
  trailing
  if length expressions > 1
    then return (Tuple expressions)
    else fail "tuple has minimum size of 2"

identifierThenSpace = do
  i <- identifier
  spaces
  return i

proc = do
  char '{'
  mparams <- optionMaybe . try $ do
    spaces
    args <- sepBy1 identifierThenSpace listSep
    spaces
    char '|'
    return args
  body <- procbody
  char '}'
  trailing
  return (Proc (fromMaybe [] mparams) body)

procbody = do
  skipMany whitespace
  optional statSep
  sepEndBy statement statSep

record = do
  string "record"
  optional whitespace
  char '{'
  spaces
  let p = do
          name <- identifier
          spaces
          char ':'
          spaces
          e <- expression
          return (name, e)
  fields <- sepEndBy p statSep
  spaces
  char '}'
  trailing
  return (Record fields)
  

letrec = do
  string "letrec"
  spaces
  char '('
  spaces
  e <- expression
  spaces
  char ')'
  spaces
  char '{'
  spaces
  let p = do
            name <- identifier
            spaces
            char ':'
            spaces
            e <- expression
            return (name, e)
  bindings <- sepEndBy p statSep
  char '}'
  trailing
  return (Letrec e bindings)

ifexpr = do
  string "if"
  spaces
  char '{'
  spaces
  let p = do
            e1 <- expression
            string "->"
            spaces
            e2 <- expression
            return (e1,e2)
  cases <- sepEndBy p statSep
  maybeElse <- optionMaybe $ do
    string "else"
    spaces
    string "->"
    spaces
    e <- expression
    optional statSep
    return e
  char '}'
  trailing
  return (If cases maybeElse)

pattern = do
  p <- choice
    [
      fmap PatLit literal,
      fmap PatVar identifier,
      fmap PatTup $ do
        char '('
        spaces
        pats <- sepBy1 pattern listSep
        spaces
        char ')'
        return pats
    ]
  trailing
  return p
  

caseexpr = do
  string "case"
  spaces
  char '('
  spaces
  e <- expression
  spaces
  char ')'
  spaces
  char '{'
  spaces
  let p = do
            pats <- sepBy1 pattern whitespace
            spaces
            string "->"
            spaces
            e <- expression
            return (pats, e)
  cases <- sepEndBy p statSep
  char '}' 
  trailing
  return (Case e cases)

basecase = do
  string "case"
  spaces
  char '{'
  spaces
  let p = do
            pats <- sepBy1 pattern whitespace
            spaces
            string "->"
            spaces
            e <- expression
            return (pats, e)
  cases <- sepEndBy p statSep
  char '}' 
  trailing
  return (BareCase cases)

fieldLeftArrow = do
  lhs <- expression
  string "<-"
  spaces
  rhs <- expression
  case lhs of
    Dot e field -> return (FieldLeftArrow e field rhs)
    _ -> fail "lhs must end in .field"

bracketLeftArrow = do
  lhs <- expression
  string "<-"
  spaces
  rhs <- expression
  case lhs of
    Bracket e1 e2 -> return (BracketLeftArrow e1 e2 rhs)
    _ -> fail "lhs must end in square brackets"

  

-- import
-- x = e
-- e
-- a.b <- c
-- a[b] <- c

-- 123
-- 123.0
-- "abc"
-- true
-- false
-- null
-- []
-- [1,2,3]
-- (1,2,3)
-- record {a: 1; b: 2}
-- a.b
-- a[b]
-- {x|x+1}
-- {$1+$2}
-- case (x) {1 -> 2; 2 -> 1}
-- case {1 -> 2; 2 -> 1}
-- letrec (x) {a: 1; b: 2}
-- if {false -> 1; true -> 2}
