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
  | [x] == "\"" =
      let (tokens@[_, _, delim], afterTokens) = lexStr i all
      in tokens ++ lex' (end delim) afterTokens
  | otherwise = token : lex' (end token) afterToken
  where
    (token, afterToken)
      | isDigit x = lexNumber i all
      | isAlpha x = lexAlphaKeyword i all
      | otherwise = lexOperatorOrPunctuation i all


lexStr :: Int -> String -> ([Token], String)
lexStr i (delim:afterOpener) =
  ([openingDelimToken, contentToken, closingDelimToken], afterCloser)
    -- Parse content after the opening string delimiter, returning as much as possible of
    -- 1. String content
    -- 2. String closing delimiter
    -- 3. Tokens after string
  where
    (contentStr, (delim:afterCloser)) = break (== delim) afterOpener
    contentStart = i + 1
    contentEnd = start + length contentStr

    openingDelimToken = Token Symbol.StrBound [delim] i contentStart
    contentToken = Token Symbol.String contentStr contentStart contentEnd
    closingDelimToken =
      Token Symbol.StrBound [delim] contentEnd (contentEnd + 1)

lexNumber :: Int -> String -> (Token, String)
lexNumber i str = (Token Symbol.Number numStr i end, afterNum)
  where
    (numStr, afterNum) = getNumber "" str False
    end = i + length numStr
    getNumber :: String -> Bool -> (String, String)
    -- Lexes a number that may have a decimal. The third parameter specifies whether a decimal has
    -- already been seen in the number.
    getNumber str seenDecimal
      | x == '.' && not seenDecimal = (digits ++ "." ++ decimals, afterDecimals)
      | otherwise = (digits, afterDigits)
      where
        (digits, afterDigits@(x:xs)) = span isDigit str
        (decimals, afterDecimals) = span isDigit afterDigits


lexAlphaKeyword :: Int -> String -> (Token, String)
lexAlphaKeyword i str = (Token symbol keyword i end, afterKeyword)
  where
    (keyword, afterKeyword) = span (isAnyOf [isAlphaNum, (== '_')]) str
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

lexOperatorOrPunctuation :: Int -> String -> (Token, String)
lexOperatorOrPunctuation i str = (Token symbol tok i end, remainder)
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
