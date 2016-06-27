module Syntax where

type Name = String


data Expr
  = Float Double
  | Int Integer
  | String String
  | Identifier Name
  | Lambda [Name] Expr
  | Var Name Expr
  | BinOp Op Expr Expr
  deriving (Eq, Ord, Show)


data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)