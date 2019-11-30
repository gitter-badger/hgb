module Symbol where

import Data.List
import Data.Map
import Data.Set

import qualified Builtin

data Symbol
  = IterUpTo
  | NEq
  | GEq
  | LEq
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
  | GT
  | LT
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | CharBound
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

keywordToSymbol :: String -> Symbol
keywordToSymbol Builtin.KeywordIterUpTo = Symbol.IterUpTo
keywordToSymbol Builtin.KeywordNEq = Symbol.NEq
keywordToSymbol Builtin.KeywordGEq = Symbol.GEq
keywordToSymbol Builtin.KeywordLEq = Symbol.LEq
keywordToSymbol Builtin.KeywordEq = Symbol.Eq
keywordToSymbol Builtin.KeywordDot = Symbol.Dot
keywordToSymbol Builtin.KeywordLBracket = Symbol.LBracket
keywordToSymbol Builtin.KeywordRBracket = Symbol.RBracket
keywordToSymbol Builtin.KeywordLParen = Symbol.LParen
keywordToSymbol Builtin.KeywordRParen = Symbol.RParen
keywordToSymbol Builtin.KeywordLBrace = Symbol.LBrace
keywordToSymbol Builtin.KeywordRBrace = Symbol.RBrace
keywordToSymbol Builtin.KeywordTypeDelim = Symbol.TypeDelim
keywordToSymbol Builtin.KeywordValueDelim = Symbol.ValueDelim
keywordToSymbol Builtin.KeywordAssign = Symbol.Assign
keywordToSymbol Builtin.KeywordGT = Symbol.GT
keywordToSymbol Builtin.KeywordLT = Symbol.LT
keywordToSymbol Builtin.KeywordPlus = Symbol.Plus
keywordToSymbol Builtin.KeywordMinus = Symbol.Minus
keywordToSymbol Builtin.KeywordTimes = Symbol.Times
keywordToSymbol Builtin.KeywordDiv = Symbol.Div
keywordToSymbol Builtin.KeywordMod = Symbol.Mod
keywordToSymbol Builtin.KeywordCharBound = Symbol.CharBound
keywordToSymbol Builtin.KeywordExprEnd = Symbol.ExprEnd
keywordToSymbol Builtin.KeywordIn = Symbol.In
keywordToSymbol Builtin.KeywordAnd = Symbol.And
keywordToSymbol Builtin.KeywordNot = Symbol.Not
keywordToSymbol Builtin.KeywordOr = Symbol.Or
keywordToSymbol Builtin.KeywordFor = Symbol.For
keywordToSymbol Builtin.KeywordWhile = Symbol.While
keywordToSymbol Builtin.KeywordIf = Symbol.If
keywordToSymbol Builtin.KeywordElse = Symbol.Else
keywordToSymbol Builtin.KeywordElseIf = Symbol.ElseIf
keywordToSymbol Builtin.KeywordFunc = Symbol.Func
keywordToSymbol Builtin.KeywordReturn = Symbol.Return
keywordToSymbol Builtin.KeywordInt8 = Symbol.Type
keywordToSymbol Builtin.KeywordInt16 = Symbol.Type
keywordToSymbol Builtin.KeywordInt32 = Symbol.Type
keywordToSymbol Builtin.KeywordInt64 = Symbol.Type
keywordToSymbol Builtin.KeywordInt = Symbol.Type
keywordToSymbol Builtin.KeywordUInt8 = Symbol.Type
keywordToSymbol Builtin.KeywordUInt16 = Symbol.Type
keywordToSymbol Builtin.KeywordUInt32 = Symbol.Type
keywordToSymbol Builtin.KeywordUInt64 = Symbol.Type
keywordToSymbol Builtin.KeywordUInt = Symbol.Type
keywordToSymbol Builtin.KeywordChar = Symbol.Type
keywordToSymbol Builtin.KeywordVoid = Symbol.Type
keywordToSymbol str = Symbol.Name
