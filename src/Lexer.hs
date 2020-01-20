module Lexer where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)
import Prelude hiding (lex)

import Symbol (Symbol)
import qualified Symbol (Symbol(..))
import Token (Token(..))

lex :: String -> [Token]
lex = lex' 0

lex' :: Int -> String -> [Token]
lex' _ "" = []
lex' i all@(x:xs)
  | isSpace x = lex' (i + 1) xs
  | [x] == "\"" = lexStr i all
  | isDigit x = lexNumber i all
  | isAlpha x = lexAlphaKeyword i all
  | otherwise = lexOperatorOrPunctuation i all

lexStr :: Int -> String -> [Token]
lexStr i (delim:xs) =
  Token Symbol.StrBound [delim] i start : contentToken : postContent
    -- Parse content after the opening string delimiter, returning as much as possible of
    -- 1. String content
    -- 2. String closing delimiter
    -- 3. Tokens after string
  where
    (contentStr, postContentStr) = break (== delim) xs
    start = i + 1
    end = start + length contentStr
    contentToken = Token Symbol.String contentStr start end
    postContent =
      case postContentStr of
        (endDelim:remainder) ->
          Token Symbol.StrBound [endDelim] end (end + 1) :
          lex' (end + 1) remainder
        _ -> []

lexNumber :: Int -> String -> [Token]
lexNumber i str = Token Symbol.Number numStr i end : lex' end remainder
  where
    (numStr, remainder) = span isDigit str
    end = i + length numStr

lexAlphaKeyword :: Int -> String -> [Token]
lexAlphaKeyword i str = Token symbol keyword i end : lex' end remainder
  where
    (keyword, remainder) = span (isAnyOf [isAlphaNum, (== '_')]) str
    end = i + length keyword
    symbol =
      case keyword of
        "in" -> Symbol.In
        "and" -> Symbol.And
        "not" -> Symbol.Not
        "or" -> Symbol.Or
        "for" -> Symbol.For
        "while" -> Symbol.While
        "if" -> Symbol.If
        "else" -> Symbol.Else
        "elseif" -> Symbol.ElseIf
        "fun" -> Symbol.Func
        "return" -> Symbol.Return
        "int8" -> Symbol.Type
        "int16" -> Symbol.Type
        "int32" -> Symbol.Type
        "int64" -> Symbol.Type
        "int" -> Symbol.Type
        "uint8" -> Symbol.Type
        "uint16" -> Symbol.Type
        "uint32" -> Symbol.Type
        "uint64" -> Symbol.Type
        "uint" -> Symbol.Type
        "double" -> Symbol.Type
        "void" -> Symbol.Type
        _ -> Symbol.Name

lexOperatorOrPunctuation :: Int -> String -> [Token]
lexOperatorOrPunctuation i str = Token symbol tok i end : lex' end remainder
  where
    (symbol, tok, remainder) = getSymbol "" str
    end = i + length tok
    getSymbol :: String -> String -> (Symbol, String, String)
    getSymbol main [] = (Symbol.Invalid, main, [])
    getSymbol main (next:rest)
      | not (isSymbol next || isPunctuation next) = (Symbol.Invalid, tok, rest)
      | otherwise =
        case tok of
          "->" -> (Symbol.IterUpTo, tok, rest)
          "-" ->
            case getSymbol tok rest of
              (Symbol.Invalid, _, _) -> (Symbol.Minus, tok, rest)
              result -> result
          "=/=" -> (Symbol.NEq, tok, rest)
          "=/" ->
            case getSymbol tok rest of
              (Symbol.Invalid, _, _) -> (Symbol.Invalid, tok, rest)
              result -> result
          "==" -> (Symbol.Eq, tok, rest)
          "=" ->
            case getSymbol tok rest of
              (Symbol.Invalid, _, _) -> (Symbol.Assign, tok, rest)
              result -> result
          ">=" -> (Symbol.GEq, tok, rest)
          ">" ->
            case getSymbol tok rest of
              (Symbol.Invalid, _, _) -> (Symbol.GT, tok, rest)
              result -> result
          "<=" -> (Symbol.LEq, tok, rest)
          "<" ->
            case getSymbol tok rest of
              (Symbol.Invalid, _, _) -> (Symbol.LT, tok, rest)
              result -> result
          "." -> (Symbol.Dot, tok, rest)
          "[" -> (Symbol.LBracket, tok, rest)
          "]" -> (Symbol.RBracket, tok, rest)
          "(" -> (Symbol.LParen, tok, rest)
          ")" -> (Symbol.RParen, tok, rest)
          "{" -> (Symbol.LBrace, tok, rest)
          "}" -> (Symbol.RBrace, tok, rest)
          ":" -> (Symbol.TypeDelim, tok, rest)
          "," -> (Symbol.ValueDelim, tok, rest)
          "+" -> (Symbol.Plus, tok, rest)
          "*" -> (Symbol.Times, tok, rest)
          "/" -> (Symbol.Div, tok, rest)
          "%" -> (Symbol.Mod, tok, rest)
          "!" -> (Symbol.ExprEnd, tok, rest)
          _ -> (Symbol.Invalid, tok, rest)
      where
        tok = main ++ [next]

isAnyOf :: [c -> Bool] -> c -> Bool
isAnyOf p c = any ($c) p
