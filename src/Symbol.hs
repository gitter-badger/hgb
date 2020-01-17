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

keywordToSymbol :: String -> Maybe Symbol
keywordToSymbol Builtin.KeywordIterUpTo = Just Symbol.IterUpTo
keywordToSymbol Builtin.KeywordNEq = Just Symbol.NEq
keywordToSymbol Builtin.KeywordGEq = Just Symbol.GEq
keywordToSymbol Builtin.KeywordLEq = Just Symbol.LEq
keywordToSymbol Builtin.KeywordEq = Just Symbol.Eq
keywordToSymbol Builtin.KeywordDot = Just Symbol.Dot
keywordToSymbol Builtin.KeywordLBracket = Just Symbol.LBracket
keywordToSymbol Builtin.KeywordRBracket = Just Symbol.RBracket
keywordToSymbol Builtin.KeywordLParen = Just Symbol.LParen
keywordToSymbol Builtin.KeywordRParen = Just Symbol.RParen
keywordToSymbol Builtin.KeywordLBrace = Just Symbol.LBrace
keywordToSymbol Builtin.KeywordRBrace = Just Symbol.RBrace
keywordToSymbol Builtin.KeywordTypeDelim = Just Symbol.TypeDelim
keywordToSymbol Builtin.KeywordValueDelim = Just Symbol.ValueDelim
keywordToSymbol Builtin.KeywordAssign = Just Symbol.Assign
keywordToSymbol Builtin.KeywordGT = Just Symbol.GT
keywordToSymbol Builtin.KeywordLT = Just Symbol.LT
keywordToSymbol Builtin.KeywordPlus = Just Symbol.Plus
keywordToSymbol Builtin.KeywordMinus = Just Symbol.Minus
keywordToSymbol Builtin.KeywordTimes = Just Symbol.Times
keywordToSymbol Builtin.KeywordDiv = Just Symbol.Div
keywordToSymbol Builtin.KeywordMod = Just Symbol.Mod
keywordToSymbol Builtin.KeywordExprEnd = Just Symbol.ExprEnd
keywordToSymbol Builtin.KeywordIn = Just Symbol.In
keywordToSymbol Builtin.KeywordAnd = Just Symbol.And
keywordToSymbol Builtin.KeywordNot = Just Symbol.Not
keywordToSymbol Builtin.KeywordOr = Just Symbol.Or
keywordToSymbol Builtin.KeywordFor = Just Symbol.For
keywordToSymbol Builtin.KeywordWhile = Just Symbol.While
keywordToSymbol Builtin.KeywordIf = Just Symbol.If
keywordToSymbol Builtin.KeywordElse = Just Symbol.Else
keywordToSymbol Builtin.KeywordElseIf = Just Symbol.ElseIf
keywordToSymbol Builtin.KeywordFunc = Just Symbol.Func
keywordToSymbol Builtin.KeywordReturn = Just Symbol.Return
keywordToSymbol Builtin.KeywordInt8 = Just Symbol.Type
keywordToSymbol Builtin.KeywordInt16 = Just Symbol.Type
keywordToSymbol Builtin.KeywordInt32 = Just Symbol.Type
keywordToSymbol Builtin.KeywordInt64 = Just Symbol.Type
keywordToSymbol Builtin.KeywordInt = Just Symbol.Type
keywordToSymbol Builtin.KeywordUInt8 = Just Symbol.Type
keywordToSymbol Builtin.KeywordUInt16 = Just Symbol.Type
keywordToSymbol Builtin.KeywordUInt32 = Just Symbol.Type
keywordToSymbol Builtin.KeywordUInt64 = Just Symbol.Type
keywordToSymbol Builtin.KeywordUInt = Just Symbol.Type
keywordToSymbol Builtin.KeywordDouble = Just Symbol.Type
keywordToSymbol Builtin.KeywordVoid = Just Symbol.Type
keywordToSymbol str = Nothing
