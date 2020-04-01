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

backslash = '\\'

endOfFileToken :: Int -> Token
endOfFileToken i = Token Symbol.EndOfFile "<EOF>" (Span i i)

lex :: String -> [Token]
lex = lex' 0

lex' :: Int -> String -> [Token]
lex' i "" = [endOfFileToken i]
lex' i all@(x:xs)
  | isSpace x = lex' (i + 1) xs
  | [x] == "\"" =
    let (tokens, afterTokens) = lexStr i all
        endIndex = end $ sourceSpan (last tokens)
     in tokens ++ lex' endIndex afterTokens
  | otherwise =
    let (token, afterToken)
          | isDigit x = lexNumber i all
          | isAlpha x = lexAlphaKeyword i all
          | otherwise = lexOperatorOrPunctuation i all
     in token : lex' (end $ sourceSpan token) afterToken

lexStr :: Int -> String -> ([Token], String)
lexStr strStart (delim:afterStrOpen) =
  case afterContent of
    [] -> ([strOpenToken, contentToken], afterContent)
    (_:afterCloser) ->
      ([strOpenToken, contentToken, strCloseToken], afterCloser)
  where
    contentStart = strStart + 1
    (contentEnd, content, afterContent) = lexStr' contentStart afterStrOpen
    contentToken = Token Symbol.String content (Span contentStart contentEnd)
    strOpenToken = Token Symbol.StrBound [delim] (Span strStart contentStart)
    strCloseToken =
      Token Symbol.StrBound [delim] (Span contentEnd (contentEnd + 1))
    lexStr' :: Int -> String -> (Int, String, String)
    lexStr' i [] = (i, [], [])
    lexStr' i xs@(ch:xs')
      | ch == delim = (i, [], xs)
      | ch == backslash =
        let (escChar, rest) = lexEscapeSequence xs'
            (end, moreContent, afterContent) = lexStr' (i + 1) rest
         in (end, escChar : moreContent, afterContent)
      | otherwise =
        let (end, moreContent, afterContent) = lexStr' (i + 1) xs'
         in (end, ch : moreContent, afterContent)

lexEscapeSequence :: String -> (Char, String)
-- There is nothing in the escape sequence, so just return the escape character.
-- TODO: this should actually be a lexing error.
lexEscapeSequence [] = (backslash, "")
lexEscapeSequence (c:rest) = (content, rest)
  where
    content =
      case c of
        'n' -> '\n'
        't' -> '\t'
        other -> other

lexNumber :: Int -> String -> (Token, String)
lexNumber i str = (Token Symbol.Number numStr (Span i end), afterNum)
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

lexAlphaKeyword :: Int -> String -> (Token, String)
lexAlphaKeyword i str = (Token symbol keyword (Span i end), afterKeyword)
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

type SymbolWithContent = (Symbol, String, String)

lexOperatorOrPunctuation :: Int -> String -> (Token, String)
lexOperatorOrPunctuation i str = (Token symbol tok (Span i end), remainder)
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
        "!" -> (Symbol.LineDelim, tok, rest)
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
