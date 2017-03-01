module Example0 where

import GFLight
import Prelude hiding ((++))

----------------------------------------------------------------------------------

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Var String
 deriving ( Show )

lin :: Expr -> String
lin (Add a b) = "(" ++ lin a ++ "+" ++ lin b ++ ")"
lin (Mul a b) = "(" ++ lin a ++ "*" ++ lin b ++ ")"
lin (Var x)   = x

expr :: P Expr
expr = choice [ Unit Add `App` expr `App` expr
              , Unit Mul `App` expr `App` expr
              , Unit Var `App` name
              ]

name :: P String
name = choice [ Unit (c:) `App` name0 | c <- ['a'..'z'] ]
 where
  name0 = choice [ Unit [], name ]

-- parse expr lin
