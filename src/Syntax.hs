module Syntax where

type Identifier = String


data Expr
  = Float Double
  | Int Integer
  | Lambda [Identifier] Expr
  | Var Identifier Expr
  | BinOp Op Expr Expr
  deriving (Eq, Ord, Show)


data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)