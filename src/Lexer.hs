module Lexer where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)
import Prelude hiding (lex)

import qualified Builtin
import Symbol (Symbol(..), keywordToSymbol)
import Token (Token(..))

lex :: String -> [Token]
lex = lex' 0

lex' :: Int -> String -> [Token]
lex' _ "" = []
lex' i all@(x:xs)
  | isSpace x = lex' (i + 1) xs
  | [x] == Builtin.KeywordStrBound = lexStr i all
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
          Token Symbol.StrBound [endDelim] end (end + 1)
            : lex' (end + 1) remainder
        _ -> []

lexNumber :: Int -> String -> [Token]
lexNumber i str =
  Token Symbol.Number numStr i end : lex' end remainder
  where
    (numStr, remainder) = span isDigit str
    end = i + length numStr

lexAlphaKeyword :: Int -> String -> [Token]
lexAlphaKeyword i str =
  Token symbol keyword i end : lex' end remainder
  where
    (keyword, remainder) = span (isAnyOf [isAlphaNum, (=='_')]) str
    end = i + length keyword
    symbol = case keywordToSymbol keyword of
      Just s -> s
      _ -> Symbol.Name

lexOperatorOrPunctuation :: Int -> String -> [Token]
lexOperatorOrPunctuation i str =
  Token symbol keyword i end : lex' end remainder
  where
    (keyword, remainder) = span (isAnyOf [isSymbol, isPunctuation]) str
    end = i + length keyword
    symbol = case keywordToSymbol keyword of
      Just s -> s
      _ -> Symbol.Invalid

isAnyOf :: [c -> Bool] -> c -> Bool
isAnyOf p c = any ($c) p
