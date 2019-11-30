module Data where

import Data.Char hiding (drop, take)
import Data.List hiding (drop, take)

import qualified Builtin
import Symbol (Symbol(..), keywordToSymbol)

data Token =
  Token Symbol String
  deriving (Eq, Show)

hgbLex :: String -> [Token]
hgbLex "" = []
hgbLex all@(x:xs)
  | isSpace (x) = hgbLex xs
  | [x] == Builtin.KeywordStrBound = lexStr all
  | isDigit x = lexNumber all
  | isAlpha x = lexAlphaKeyword all
  | otherwise = lexOperatorOrPunctuation all

lexStr :: String -> [Token]
lexStr (delim:xs) = (Token Symbol.StrBound [delim]) : content ++ postContent
    -- Parse content after the opening string delimiter, returning as much as possible of
    -- 1. String content
    -- 2. String closing delimiter
    -- 3. Tokens after string
  where
    (contentStr, postContentStr) = break (== delim) xs
    content =
      case contentStr of
        [] -> []
        otherwise -> [(Token Symbol.String contentStr)]
    postContent =
      case postContentStr of
        [] -> []
        (endDelim:remainder) ->
          (Token Symbol.StrBound [endDelim]) : hgbLex remainder

lexNumber :: String -> [Token]
lexNumber str = (Token Symbol.Number dig) : hgbLex remainder
  where
    (dig, remainder) = span isDigit str

lexAlphaKeyword :: String -> [Token]
lexAlphaKeyword str =
  (Token (keywordToSymbol keyword) keyword) : hgbLex remainder
  where
    (keyword, remainder) = span isAlphaNum str

lexOperatorOrPunctuation :: String -> [Token]
lexOperatorOrPunctuation str = (Token symbol keyword) : hgbLex remainder
  where
    (keyword, remainder) = span (isAnyOf [isSymbol, isPunctuation]) str
    potentialSymbol = keywordToSymbol keyword
    symbol =
      case potentialSymbol of
        Symbol.Name -> Symbol.Invalid
        otherwise -> potentialSymbol

isAnyOf :: [c -> Bool] -> c -> Bool
isAnyOf p c = or $ map ($c) p
