module Symbol
  ( Symbol(..)
  , symbolToStr
  ) where

import Data.Function ((&))
import Data.List

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
  | CharBound
  | LineDelim
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
  | Char
  | Number
  | Name
  | EndOfFile
  -- TODO(ayazhafiz): it would be nicer if we showed the expected symbol token rather than the
  -- symbol name.
  deriving (Eq, Show)

symbolToStr :: Symbol -> String
symbolToStr LineDelim = "!"
symbolToStr RParen = ")"
symbolToStr ValueDelim = ","
symbolToStr TypeDelim = ":"
symbolToStr RBracket = "]"
symbolToStr s = "<" ++ show s ++ ">" -- e.g. <Type>
