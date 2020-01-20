module Symbol where

import Data.List
import Data.Map
import Data.Set

data Symbol
  = IterUpTo
  | NEq
  | GEq
  | LEq
  | GT
  | LT
  | Eq
  | Dot
  | LBracket
  | RBracket
  | LParen
  | RParen
  | LBrace
  | RBrace
  | TypeDelim
  | ValueDelim
  | Assign
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | StrBound
  | ExprEnd
  | In
  | And
  | Not
  | Or
  | For
  | While
  | If
  | Else
  | ElseIf
  | Func
  | Return
  | Type
  | Invalid
  | String
  | Number
  | Name
  deriving (Eq, Show)
