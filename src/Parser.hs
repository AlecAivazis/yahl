module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOperator f)) assoc

--table = [[binary "*" Times Ex.AssocLeft,
--          binary "/" Divide Ex.AssocLeft]
--        ,[binary "+" Plus Ex.AssocLeft,
--          binary "-" Minus Ex.AssocLeft]]

table = []

binop = Ex.Infix (BinaryOperator <$> operator) Ex.AssocLeft

expr :: Parser Expr
expr = Ex.buildExpressionParser (table ++ [[binop]]) factor


int :: Parser Expr
int = do
  n <- integer
  return $ Int n


floating :: Parser Expr
floating = do
  n <- float
  return $ Float n


binarydef :: Parser Expr
binarydef = do
  o <- operator
  arg1 <- identifier
  arg2 <- identifier
  reserved "->"
  body <- expr
  return $ BinaryDef o arg1 arg2 body


binaryOp :: Parser Expr
binaryOp = do
  target1 <- expr
  operator <- operator
  target2 <- expr
  return $ BinaryOperator operator target1 target2


stringLiteral :: Parser Expr
stringLiteral = do
  s <- stringLit
  return $ String s


function :: Parser Expr
function = do
  name <- identifier
  args <- many identifier
  reserved "->"
  body <- expr
  return $ Var name $ Lambda args body


lambda :: Parser Expr
lambda = do
  reserved "λ"
  args <- many identifier
  reserved "->"
  body <- expr
  return $ Lambda args body


factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try stringLiteral
      <|> try lambda
      <|> try function
      <|> try binarydef
      <|> try variable


toplevel :: Parser [Expr]
toplevel = many $ do
    exp <- expr
    return exp


variable :: Parser Expr
variable = do
  name <- identifier
  return $ Identifier name


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s