module Lexer
  ( lex
  ) where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)
import Prelude hiding (lex)

import Symbol (Symbol)
import qualified Symbol (Symbol(..))
import Token (Token(..))
import Utils

endOfFileToken :: Int -> Token
endOfFileToken i = Token Symbol.EndOfFile "" i i

type SymbolWithContent = (Symbol, String, String)

lexOperatorOrPunctuation :: Int -> String -> (Token, String)
lexOperatorOrPunctuation i str = (Token symbol tok i end, remainder)
  where
    (symbol, tok, remainder) = getSymbol "" str
    end = i + length tok
    getSymbol :: String -> String -> SymbolWithContent
    getSymbol main (next:rest) =
      case tok of
        "->" -> (Symbol.IterUpTo, tok, rest)
        "-" -> getSymbol `backupInvalid` (Symbol.Minus, tok, rest)
        "=/=" -> (Symbol.NEq, tok, rest)
        "=/" -> getSymbol tok rest
        "==" -> (Symbol.Eq, tok, rest)
        "=" -> getSymbol `backupInvalid` (Symbol.Assign, tok, rest)
        ">=" -> (Symbol.GEq, tok, rest)
        ">" -> getSymbol `backupInvalid` (Symbol.GT, tok, rest)
        "<=" -> (Symbol.LEq, tok, rest)
        "<" -> getSymbol `backupInvalid` (Symbol.LT, tok, rest)
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
    backupInvalid ::
         (String -> String -> SymbolWithContent)
      -> SymbolWithContent
      -> SymbolWithContent
    backupInvalid _ backup@(_, _, []) = backup
    backupInvalid getSymbol backup@(_, contentToken, contentRest) =
      case getSymbol contentToken contentRest of
        (Symbol.Invalid, _, _) -> backup
        token -> token

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

lexNumber :: Int -> String -> (Token, String)
lexNumber i str = (Token Symbol.Number numStr i end, afterNum)
  where
    (numStr, afterNum) = getNumber str
    end = i + length numStr
    getNumber :: String -> (String, String)
    -- Lexes a number that may have a decimal. The third parameter specifies whether a decimal has
    -- already been seen in the number.
    getNumber str =
      case afterDigits of
        ('.':afterDot) -> (digits ++ "." ++ decimals, afterDecimals)
          where (decimals, afterDecimals) = span isDigit afterDot
        _ -> (digits, afterDigits)
      where
        (digits, afterDigits) = span isDigit str

lexStr :: Int -> String -> ([Token], String)
lexStr i (delim:afterOpener) =
  case afterContent of
    (_:afterCloser) ->
      ([openingDelimToken, contentToken, closingDelimToken], afterCloser)
    _ -> ([openingDelimToken, contentToken], afterContent)
  where
    (contentStr, afterContent) = break (== delim) afterOpener
    contentStart = i + 1
    contentEnd = contentStart + length contentStr
    openingDelimToken = Token Symbol.StrBound [delim] i contentStart
    contentToken = Token Symbol.String contentStr contentStart contentEnd
    closingDelimToken =
      Token Symbol.StrBound [delim] contentEnd (contentEnd + 1)

lex :: String -> [Token]
lex = lex' 0
  where
    lex' :: Int -> String -> [Token]
    lex' i "" = [endOfFileToken i]
    lex' i all@(x:xs)
      | isSpace x = lex' (i + 1) xs
      | x == '\"' =
        let (tokens, afterTokens) = lexStr i all
            endIndex = end (last tokens)
         in tokens ++ lex' endIndex afterTokens
      | otherwise =
        let (token, afterToken)
              | isDigit x = lexNumber i all
              | isAlpha x = lexAlphaKeyword i all
              | otherwise = lexOperatorOrPunctuation i all
         in token : lex' (end token) afterToken
