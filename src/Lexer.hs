module Lexer where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)
import Prelude hiding (lex)

import qualified Builtin
import Symbol (Symbol(..), keywordToSymbol)
import Token (Token(..))

lex :: String -> [Token]
lex "" = []
lex all@(x:xs)
  | isSpace (x) = lex xs
  | [x] == Builtin.KeywordStrBound = lexStr all
  | isDigit x = lexNumber all
  | isAlpha x = lexAlphaKeyword all
  | otherwise = lexOperatorOrPunctuation all

lexStr :: String -> [Token]
lexStr (delim:xs) = (Token Symbol.StrBound [delim]) : content : postContent
    -- Parse content after the opening string delimiter, returning as much as possible of
    -- 1. String content
    -- 2. String closing delimiter
    -- 3. Tokens after string
  where
    (contentStr, postContentStr) = break (== delim) xs
    content = (Token Symbol.String contentStr)
    postContent =
      case postContentStr of
        [] -> []
        (endDelim:remainder) ->
          (Token Symbol.StrBound [endDelim]) : lex remainder

lexNumber :: String -> [Token]
lexNumber str = (Token Symbol.Number numStr) : lex remainder
  where
    (numStr, remainder) = span isDigit str

lexAlphaKeyword :: String -> [Token]
lexAlphaKeyword str = (Token (keywordToSymbol keyword) keyword) : lex remainder
  where
    (keyword, remainder) = span isAlphaNum str

lexOperatorOrPunctuation :: String -> [Token]
lexOperatorOrPunctuation str = (Token symbol keyword) : lex remainder
  where
    (keyword, remainder) = span (isAnyOf [isSymbol, isPunctuation]) str
    potentialSymbol = keywordToSymbol keyword
    symbol =
      case potentialSymbol of
        Symbol.Name -> Symbol.Invalid
        otherwise -> potentialSymbol

isAnyOf :: [c -> Bool] -> c -> Bool
isAnyOf p c = or $ map ($c) p
