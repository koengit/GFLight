module Main where

import Parse

--------------------------------------------------------------------------------

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var Char

pExpr :: Set Expr
pExpr = Union [ Unit Add `App` pExpr `App` pExpr
              , Unit Mul `App` pExpr `App` pExpr
              , Unit Var `App` Union [ Unit c | c <- ['a'..'z'] ]
              ]

instance Show Expr where
  show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Num n)   = show n

s1 = "(((a+b)+c)*(a+b))*(c+c)"



