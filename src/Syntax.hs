module Syntax where

type Name = String

type Prec = Integer


data Expr
  = Float Double
  | Int Integer
  | String String
  | Identifier Name
  | Lambda [Name] Expr
  | Var Name Expr
  | BinaryOperator Name Expr Expr
  | BinaryDef Name Name Name Expr
  deriving (Eq, Ord, Show)
