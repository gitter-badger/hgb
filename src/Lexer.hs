module Lexer where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)
import Prelude hiding (lex)


import Symbol (Symbol)
import qualified Symbol (Symbol(..))
import Token

index :: String -> IndexString
index = index' 0
  where
    index' :: Int -> String -> IndexString
    index' _ "" = []
    index' i (x:xs) = Index x i : index' (i + 1) xs

lex :: String -> [Token]
lex = (lex' . index)

lex' :: IndexString -> [Token]
lex' [] = []
lex' all@(x:xs)
  | isSpace c = lex' xs
  | c == '\"' =
    let (tokens, afterTokens) = lexStr all
     in tokens ++ lex' afterTokens
  | otherwise =
    let (token, afterToken)
          | isDigit c = lexNumber all
          | isAlpha c = lexAlphaKeyword all
          | otherwise = lexOperatorOrPunctuation all
     in token : lex' afterToken
  where
    c = val x

lexStr :: IndexString -> ([Token], IndexString)
lexStr (delim:afterOpener) =
  case afterContent of
    (_:afterCloser) ->
      ([openingDelimToken, contentToken, closingDelimToken], afterCloser)
    _ -> ([openingDelimToken, contentToken], afterContent)
  where
    (contentStr, afterContent) = break (equalVal delim) afterOpener
    openingDelimToken = Token Symbol.StrBound [delim]
    contentToken = Token Symbol.String contentStr
    closingDelimToken = Token Symbol.StrBound [delim]

lexNumber :: IndexString -> (Token, IndexString)
lexNumber str = (Token Symbol.Number numStr, afterNum)
  where
    (numStr, afterNum) = getNumber str
    getNumber :: IndexString -> (IndexString, IndexString)
    -- Lexes a number that may have a decimal. The third parameter specifies whether a decimal has
    -- already been seen in the number.
    getNumber str
      | isOfVal '.' x = (digits ++ [x] ++ decimals, afterDecimals)
      | otherwise = (digits, afterDigits)
      where
        (digits, afterDigits@(x:xs)) = span (isDigit . val) str
        (decimals, afterDecimals) = span (isDigit . val) xs

lexAlphaKeyword :: IndexString -> (Token, IndexString)
lexAlphaKeyword str = (Token symbol keyword, afterKeyword)
  where
    (keyword, afterKeyword) = span (isAnyOf [isAlphaNum, (== '_')] . val) str
    keywordStr = toString keyword
    symbol =
      case keywordStr of
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

lexOperatorOrPunctuation :: IndexString -> (Token, IndexString)
lexOperatorOrPunctuation str = (Token symbol tok, remainder)
  where
    (symbol, tok, remainder) = getSymbol [] str
    getSymbol :: IndexString -> IndexString -> (Symbol, IndexString, IndexString)
    getSymbol main [] = (Symbol.Invalid, main, [])
    getSymbol main (next:rest)
      | not (isAnyOf [isSymbol, isPunctuation] (val next)) = (Symbol.Invalid, tok, rest)
      | otherwise =
        case toString tok of
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
